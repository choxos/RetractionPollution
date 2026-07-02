from __future__ import annotations

from pathlib import Path
from typing import Any

from .storage import StudyStore
from .util import ensure_dir


def _require_analysis_deps():
    try:
        import matplotlib.pyplot as plt
        import networkx as nx
        import pandas as pd
    except ImportError as exc:  # pragma: no cover
        raise RuntimeError(
            "Analysis dependencies are required. Run `uv sync` or install project dependencies."
        ) from exc
    return pd, nx, plt


def run_analysis(
    store: StudyStore, output_dir: Path, *, max_analysis_depth: int = 2
) -> dict[str, Any]:
    pd, nx, plt = _require_analysis_deps()
    table_dir = ensure_dir(output_dir / "tables")
    figure_dir = ensure_dir(output_dir / "figures")
    graph_dir = ensure_dir(output_dir / "graphs")

    nodes = store.con.execute(
        """
        SELECT
            f.openalex_id,
            f.depth,
            w.doi,
            w.title,
            w.publication_date,
            w.publication_year,
            w.work_type,
            w.is_retracted,
            w.cited_by_count,
            w.source_name,
            w.topic_name,
            w.topic_domain
        FROM frontier_nodes f
        LEFT JOIN works w ON f.openalex_id = w.openalex_id
        WHERE f.depth <= ?
        """,
        [max_analysis_depth],
    ).fetchdf()
    edges = store.con.execute(
        """
        SELECT e.source_id, e.target_id, e.depth, e.source_api, e.citation_date
        FROM citation_edges e
        JOIN frontier_nodes fs ON e.source_id = fs.openalex_id
        JOIN frontier_nodes ft ON e.target_id = ft.openalex_id
        WHERE fs.depth <= ? AND ft.depth <= ?
        """,
        [max_analysis_depth, max_analysis_depth],
    ).fetchdf()
    seeds = store.con.execute("SELECT * FROM seeds").fetchdf()

    nodes.to_csv(table_dir / "nodes_depth2.csv", index=False)
    edges.to_csv(table_dir / "edges_depth2.csv", index=False)

    depth_counts = (
        nodes.groupby("depth", dropna=False)
        .size()
        .reset_index(name="node_count")
        .sort_values("depth")
    )
    depth_counts.to_csv(table_dir / "depth_counts.csv", index=False)

    top_seeds = _seed_metrics(pd, seeds, nodes, edges)
    top_seeds.to_csv(table_dir / "top_polluted_seeds.csv", index=False)

    bridges = _bridge_metrics(nodes, edges)
    bridges.to_csv(table_dir / "bridge_papers.csv", index=False)

    topics = (
        nodes[nodes["depth"].isin([1, 2])]
        .groupby(["depth", "topic_domain", "topic_name"], dropna=False)
        .size()
        .reset_index(name="node_count")
        .sort_values(["depth", "node_count"], ascending=[True, False])
    )
    topics.to_csv(table_dir / "topic_distribution.csv", index=False)

    graph = _build_graph(nx, nodes, edges)
    nx.write_graphml(graph, graph_dir / "network_depth2.graphml")

    summary = _summary_metrics(pd, nx, graph, seeds, nodes, edges, top_seeds)
    summary_df = pd.DataFrame([{"metric": key, "value": value} for key, value in summary.items()])
    summary_df.to_csv(table_dir / "summary.csv", index=False)

    _plot_depth_counts(plt, depth_counts, figure_dir)
    _plot_top_seeds(plt, top_seeds, figure_dir)
    _plot_post_notice_timeline(pd, plt, seeds, nodes, edges, figure_dir)

    return summary


def _seed_metrics(pd, seeds, nodes, edges):
    node_depth = dict(zip(nodes["openalex_id"], nodes["depth"], strict=False))
    node_dates = dict(zip(nodes["openalex_id"], nodes["publication_date"], strict=False))
    records: list[dict[str, Any]] = []
    for seed in seeds.to_dict("records"):
        seed_id = seed.get("openalex_id")
        if not seed_id:
            continue
        direct_edges = edges[edges["target_id"] == seed_id]
        direct_citers = set(direct_edges["source_id"])
        depth2_edges = edges[edges["target_id"].isin(direct_citers)]
        depth2_sources = {
            source_id for source_id in depth2_edges["source_id"] if node_depth.get(source_id) == 2
        }
        notice_date = pd.to_datetime(seed.get("notice_date"), errors="coerce")
        post_direct = 0
        post_depth2 = 0
        if not pd.isna(notice_date):
            for source_id in direct_citers:
                pub_date = pd.to_datetime(node_dates.get(source_id), errors="coerce")
                if not pd.isna(pub_date) and pub_date > notice_date:
                    post_direct += 1
            for source_id in depth2_sources:
                pub_date = pd.to_datetime(node_dates.get(source_id), errors="coerce")
                if not pd.isna(pub_date) and pub_date > notice_date:
                    post_depth2 += 1
        records.append(
            {
                "record_id": seed.get("record_id"),
                "openalex_id": seed_id,
                "title": seed.get("title"),
                "notice_type": seed.get("notice_type"),
                "notice_date": seed.get("notice_date"),
                "direct_citers": len(direct_citers),
                "depth2_descendants": len(depth2_sources),
                "total_depth2_reach": len(direct_citers.union(depth2_sources)),
                "post_notice_direct_citers": post_direct,
                "post_notice_depth2_descendants": post_depth2,
            }
        )
    if not records:
        return pd.DataFrame(
            columns=[
                "record_id",
                "openalex_id",
                "title",
                "notice_type",
                "notice_date",
                "direct_citers",
                "depth2_descendants",
                "total_depth2_reach",
                "post_notice_direct_citers",
                "post_notice_depth2_descendants",
            ]
        )
    return pd.DataFrame(records).sort_values("total_depth2_reach", ascending=False)


def _bridge_metrics(nodes, edges):
    depth_by_id = dict(zip(nodes["openalex_id"], nodes["depth"], strict=False))
    title_by_id = dict(zip(nodes["openalex_id"], nodes["title"], strict=False))
    direct_nodes = [node_id for node_id, depth in depth_by_id.items() if depth == 1]
    records = []
    for node_id in direct_nodes:
        cited_seed_count = len(
            {
                target_id
                for target_id in edges.loc[edges["source_id"] == node_id, "target_id"]
                if depth_by_id.get(target_id) == 0
            }
        )
        depth2_citer_count = len(
            {
                source_id
                for source_id in edges.loc[edges["target_id"] == node_id, "source_id"]
                if depth_by_id.get(source_id) == 2
            }
        )
        if cited_seed_count or depth2_citer_count:
            records.append(
                {
                    "openalex_id": node_id,
                    "title": title_by_id.get(node_id),
                    "cited_seed_count": cited_seed_count,
                    "depth2_citer_count": depth2_citer_count,
                }
            )
    return (
        nodes.iloc[0:0][["openalex_id", "title"]].assign(
            cited_seed_count=[], depth2_citer_count=[]
        )
        if not records
        else __import__("pandas").DataFrame(records).sort_values(
            ["depth2_citer_count", "cited_seed_count"], ascending=False
        )
    )


def _summary_metrics(pd, nx, graph, seeds, nodes, edges, top_seeds):
    resolved_seeds = int(seeds["openalex_id"].notna().sum()) if not seeds.empty else 0
    post_direct = (
        int(top_seeds["post_notice_direct_citers"].sum())
        if "post_notice_direct_citers" in top_seeds
        else 0
    )
    return {
        "seed_records": int(len(seeds)),
        "resolved_seed_records": resolved_seeds,
        "depth2_nodes": int(len(nodes)),
        "depth2_edges": int(len(edges)),
        "depth0_nodes": int((nodes["depth"] == 0).sum()) if not nodes.empty else 0,
        "depth1_nodes": int((nodes["depth"] == 1).sum()) if not nodes.empty else 0,
        "depth2_nodes_only": int((nodes["depth"] == 2).sum()) if not nodes.empty else 0,
        "weak_components": nx.number_weakly_connected_components(graph) if graph else 0,
        "post_notice_direct_citers": post_direct,
        "opencitations_edges": int((edges["source_api"] == "opencitations").sum())
        if "source_api" in edges
        else 0,
        "openalex_edges": int((edges["source_api"] == "openalex").sum())
        if "source_api" in edges
        else 0,
    }


def _build_graph(nx, nodes, edges):
    graph = nx.DiGraph()
    for node in nodes.to_dict("records"):
        node_id = node.pop("openalex_id")
        attrs = {key: _graph_attr(value) for key, value in node.items()}
        graph.add_node(node_id, **attrs)
    for edge in edges.to_dict("records"):
        graph.add_edge(
            edge["source_id"],
            edge["target_id"],
            depth=_graph_attr(edge.get("depth")),
            source_api=_graph_attr(edge.get("source_api")),
            citation_date=_graph_attr(edge.get("citation_date")),
        )
    return graph


def _graph_attr(value: Any) -> str:
    if value is None:
        return ""
    try:
        if value != value:  # NaN
            return ""
    except TypeError:
        pass
    return str(value)


def _plot_depth_counts(plt, depth_counts, figure_dir: Path) -> None:
    fig, ax = plt.subplots(figsize=(6, 4))
    ax.bar(depth_counts["depth"].astype(str), depth_counts["node_count"], color="#3b82f6")
    ax.set_xlabel("Network depth")
    ax.set_ylabel("Nodes")
    ax.set_title("Citation Pollution Network Size by Depth")
    fig.tight_layout()
    fig.savefig(figure_dir / "depth_counts.png", dpi=200)
    fig.savefig(figure_dir / "depth_counts.svg")
    plt.close(fig)


def _plot_top_seeds(plt, top_seeds, figure_dir: Path) -> None:
    if top_seeds.empty:
        return
    data = top_seeds.head(15).iloc[::-1]
    labels = [str(title)[:70] for title in data["title"]]
    fig, ax = plt.subplots(figsize=(9, 6))
    ax.barh(labels, data["total_depth2_reach"], color="#ef4444")
    ax.set_xlabel("Unique descendants through depth 2")
    ax.set_title("Most Polluted Retracted or Concerning Papers")
    fig.tight_layout()
    fig.savefig(figure_dir / "top_polluted_seeds.png", dpi=200)
    fig.savefig(figure_dir / "top_polluted_seeds.svg")
    plt.close(fig)


def _plot_post_notice_timeline(pd, plt, seeds, nodes, edges, figure_dir: Path) -> None:
    if seeds.empty or nodes.empty or edges.empty:
        return
    seed_notice = {
        row["openalex_id"]: pd.to_datetime(row["notice_date"], errors="coerce")
        for row in seeds.to_dict("records")
        if row.get("openalex_id")
    }
    node_year = dict(zip(nodes["openalex_id"], nodes["publication_year"], strict=False))
    node_date = dict(zip(nodes["openalex_id"], nodes["publication_date"], strict=False))
    years: list[int] = []
    for edge in edges.to_dict("records"):
        notice_date = seed_notice.get(edge["target_id"])
        if notice_date is None or pd.isna(notice_date):
            continue
        pub_date = pd.to_datetime(node_date.get(edge["source_id"]), errors="coerce")
        year = node_year.get(edge["source_id"])
        if not pd.isna(pub_date) and pub_date > notice_date and year:
            years.append(int(year))
    if not years:
        return
    counts = pd.Series(years).value_counts().sort_index()
    fig, ax = plt.subplots(figsize=(8, 4))
    ax.plot(counts.index, counts.values, marker="o", color="#0f766e")
    ax.set_xlabel("Publication year")
    ax.set_ylabel("Post-notice direct citations")
    ax.set_title("Post-Notice Direct Citations Over Time")
    fig.tight_layout()
    fig.savefig(figure_dir / "post_notice_timeline.png", dpi=200)
    fig.savefig(figure_dir / "post_notice_timeline.svg")
    plt.close(fig)

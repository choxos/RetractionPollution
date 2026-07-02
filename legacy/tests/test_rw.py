from pathlib import Path

from retraction_pollution.rw import is_seed_notice, load_seed_rows


def test_notice_filter_includes_retraction_and_expression_of_concern():
    assert is_seed_notice("Retraction")
    assert is_seed_notice("Expression of concern")
    assert is_seed_notice("Correction; Retraction")
    assert not is_seed_notice("Correction")


def test_load_seed_rows_filters_and_normalizes(tmp_path: Path):
    csv_path = tmp_path / "rw.csv"
    csv_path.write_text(
        "\n".join(
            [
                "Record ID,Title,RetractionNature,RetractionDate,OriginalPaperDate,"
                "OriginalPaperDOI,OriginalPaperPubMedID,Author,Journal,Publisher,Subject,"
                "Reason,ArticleType,Country",
                "1,Retracted paper,Retraction,2022-01-02,2020,https://doi.org/10.1/ABC,"
                "123,A Smith,Journal,Publisher,Biology,Error,Research,US",
                "2,Correction only,Correction,2022-01-02,2020,10.2/DEF,456,A Jones,J,P,Chem,"
                "Typo,Research,CA",
                "3,EOC paper,Expression of concern,2023-03,2021-05,Unavailable,0,A Doe,J,P,"
                "Med,Concern,Research,GB",
            ]
        ),
        encoding="utf-8",
    )

    seeds = load_seed_rows(csv_path)

    assert [seed["record_id"] for seed in seeds] == ["1", "3"]
    assert seeds[0]["original_doi"] == "10.1/abc"
    assert seeds[0]["original_pmid"] == "123"
    assert seeds[1]["original_doi"] is None
    assert seeds[1]["notice_date"] == "2023-03-01"

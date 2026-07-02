from retraction_pollution.opencitations import extract_pid, parse_open_citation


def test_extract_pid_from_opencitations_pid_string():
    pids = "omid:br/06101801781 doi:10.7717/peerj-cs.421 pmid:33817056"
    assert extract_pid(pids, "doi") == "10.7717/peerj-cs.421"
    assert extract_pid(pids, "pmid") == "33817056"
    assert extract_pid(pids, "pmcid") is None


def test_parse_open_citation_normalizes_identifiers_and_date():
    citation = parse_open_citation(
        {
            "citing": "omid:x doi:10.1000/ABC pmid:12345",
            "cited": "omid:y doi:10.2000/DEF",
            "creation": "2024-02",
        }
    )

    assert citation.citing_doi == "10.1000/abc"
    assert citation.citing_pmid == "12345"
    assert citation.cited_doi == "10.2000/def"
    assert citation.creation_date == "2024-02-01"

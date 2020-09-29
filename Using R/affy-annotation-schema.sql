-- SQLite Database Schema for Affymetrix Annotation (prototype)
--
--

-- Accession number table
CREATE TABLE acc (affy_id TEXT, -- Affymetrix probe id
                  acc_num TEXT  -- Accession number
);

-- GO id to ontology code lookup table
CREATE TABLE go_ont (go_id TEXT, -- GO id
                     ont   TEXT  -- Ontology code
);

-- GO ontology code to name lookup table
CREATE TABLE go_ont_name (ont TEXT,     -- Ontology code
                          ont_name TEXT -- Ontology name 
);

-- GO annotation table
CREATE TABLE go_probe (affy_id TEXT, -- Affymetrix probe id
                       go_id   TEXT, -- GO id
                       evi     TEXT  -- Evidence code
);

-- GO evidence code to evidence description lookup table
CREATE TABLE go_evi (evi TEXT,        -- Evidence code
                     description TEXT -- Evidence description
);

-- PubMed table
CREATE TABLE pubmed (affy_id TEXT, -- Affymetrix probe id
                     pm_id   TEXT  -- PubMed id 
);


-- sqlite shell tool specific commands follow

-- Fast way to import properly formatted text files.
-- Default separator is '|', no quotes, no row or column names.
.import hgu95av2-acc.txt acc
.import hgu95av2-goEvidence.txt go_evi
.import hgu95av2-goOntName.txt go_ont_name
.import hgu95av2-goId2Ontol.txt go_ont
.import hgu95av2-go.txt go_probe
.import hgu95av2-pmid.txt pubmed
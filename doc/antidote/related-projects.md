---
description: Related projects which make use of Antidote or extend it
---

# PotionDB

# Antidote Query Language (AQL)

A SQL-like interface for the AntidoteDB data store.

See the [repository](https://github.com/AntidoteDB/antidote_aql) for more information.

Example:

```SQL
CREATE UPDATE-WINS TABLE Student (
	StudentID INT PRIMARY KEY,
	Name VARCHAR,
	Age INT DEFAULT 18,
	YearsLeft COUNTER_INT CHECK (YearsLeft > 0),
	Passport_id INTEGER FOREIGN KEY UPDATE-WINS REFERENCES Passport(id)
) PARTITION ON (Age);
```


# EdgeAnt

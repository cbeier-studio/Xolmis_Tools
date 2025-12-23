# Official Specification of the Data Generation Schema  

**_Mock Data Generator – Xolmis Developer Tools_**

This document describes the standard format used to define table schemas in the data generator.  
Each table is defined by a JSON file containing:

- table name  
- columns  
- generation functions  
- hierarchical relationships  
- table relationships  
- polymorphic associations  
- external data sources  
- validation rules  

The only required sections are:

- `table`
- `columns`

## 1. General Schema Structure

```json
{
  "table": "table_name",
  "relations": { ... },
  "polymorphic": { ... },
  "hierarchy": { ... },
  "externalSource": { ... },
  "localitiesSource": "localities",
  "columns": [ ... ]
}
```

`additionalProperties` is not allowed at the root level — only the fields above are permitted.

## 2. `relations` Section (Parent–Child Relationship)

Defines whether the table is subordinate to another.

```json
"relations": {
  "parent": "surveys",
  "parentKey": "id",
  "foreignKey": "survey_id",
  "optionalParent": true,
  "minChildren": 0,
  "maxChildren": 10
}
```

### Available fields

| Field | Type | Description |
| --- | --- | --- |
| `parent` | string | Name of the parent table |
| `parentKey` | string | Field in the parent table used as PK |
| `foreignKey` | string | FK field in the current table |
| `optionalParent` | boolean | Allows FK to be null |
| `minChildren` | integer ≥ 0 | Minimum number of children per parent |
| `maxChildren` | integer ≥ 0 | Maximum number of children per parent |

## 3. `polymorphic` Section (Polymorphic Association)

Used for tables that reference multiple possible tables.

```json
"polymorphic": {
  "tableField": "table_name",
  "idField": "record_id",
  "allowedTables": ["captures", "nests", "eggs"]
}
```

### Fields

| Field | Type | Description |
| --- | --- | --- |
| `tableField` | string | Field storing the referenced table name |
| `idField` | string | Field storing the referenced record ID |
| `allowedTables` | string[] | List of allowed tables |

## 4. `hierarchy` Section

Used for tables with hierarchical structure (e.g., gazetteer).

```json
"hierarchy": {
  "rankField": "site_rank",
  "parentField": "parent_site_id",
  "levels": [
    { "rank": "C", "min": 1, "max": 1, "source": "countries" },
    { "rank": "S", "min": 1, "max": 5, "source": "states" }
  ]
}
```

### Fields

| Field | Type | Description |
| --- | --- | --- |
| `rankField` | string | Field defining the hierarchy level |
| `parentField` | string | Field referencing the parent |
| `levels` | array | List of hierarchical levels |

### Level structure

| Field | Type | Description |
| --- | --- | --- |
| `rank` | string or integer | Level identifier |
| `min` | integer | Minimum number of records for this level |
| `max` | integer | Maximum number of records for this level |
| `source` | string | Data source for this level |

## 5. `externalSource` Section

Allows loading data from external files.

```json
"externalSource": {
  "countriesStatesCities": {
    "file": "csc_database.json",
    "countryField": "name",
    "stateField": "state_name",
    "cityField": "city_name",
    "allowedCountries": ["Brazil", "Argentina"],
    "localitiesSource": "localities.json"
  }
}
```

Each key inside `externalSource` is an object with:

| Field | Type | Description |
| --- | --- | --- |
| `file` | string | Path to the external file |
| `countryField` | string | Country field name |
| `stateField` | string | State field name |
| `cityField` | string | City field name |
| `allowedCountries` | string[] | List of allowed countries |
| `additionalProperties` | true | Allows extra fields |
| `localitiesSource` | string | Path to the source file for locality names |

## 6. `columns` Section

Each column has the following structure:

```json
{
  "name": "age",
  "type": "integer",
  "nullable": true,
  "unique": true,
  "generationMode": "random",
  "incrementBy": 1,
  "default": 0,
  "enum": [1, 2, 3],
  "weights": [0.5, 0.3, 0.2],
  "range": ["10", "20"],
  "mask": "AA-9999",
  "source": "countries",
  "sourceTable": "users",
  "sourceField": "id",
  "filterBy": { "country": "Brazil" },
  "function": "random_int",
  "args": { "min": 1, "max": 100 },
  "dependsOn": ["birthdate"]
}
```

### General fields

| Field | Type | Description |
| --- | --- | --- |
| `name` | string | Column name |
| `type` | string | integer, float, string, boolean, date, datetime |
| `autoIncrement` | boolean | Generates automatic sequence |
| `startAt` | integer | Initial value for autoIncrement |
| `nullable` | boolean | Allows null values |
| `unique` | boolean | Allows duplicate values or not |
| `generationMode` | string | random, sequential, incremental, conditional, template, list |
| `incrementBy` | integer | Value to increment |
| `default` | any | Default value |
| `fixed` | any | Constant value |
| `ignore` | boolean | Field exists in DB but should not be generated |
| `enum` | array | List of allowed values |
| `multiple` | boolean | Allows multiple values |
| `weights` | number[] | Weights for enum selection |
| `range` | [string, string] | Range for dates or numbers |
| `mask` | string | Formatting mask |
| `source` | string | Data source |
| `sourceTable` | string | Source table |
| `sourceField` | string | Source field |
| `filterBy` | object | Filters applied to the source |
| `function` | string | Generation function |
| `args` | object | Function arguments |
| `dependsOn` | string[] | Fields required before generation |

## 7. Supported Functions

### Date Functions

| Function | Description |
| --- | --- |
| `add_days_random` | Adds a random number of days to a base date. |
| `add_years_random` | Adds a random number of years to a base date. |
| `inherit_or_random_date` | Inherits a date from another field or generates a random one if missing. |
| `extract_day` | Extracts the day component from a date. |
| `extract_month` | Extracts the month component from a date. |
| `extract_year` | Extracts the year component from a date. |
| `days_after` | Generates a date a certain number of days after another date. |

### Time Functions

| Function | Description |
| --- | --- |
| `random_time` | Generates a random time of day. |
| `random_time_after` | Generates a random time after a reference time. |
| `random_time_optional` | Same as `random_time`, but may return null based on probability. |
| `random_time_after_optional` | Same as `random_time_after`, but may return null. |

### Number Functions

| Function | Description |
| --- | --- |
| `random_int` | Generates a random integer. |
| `random_int_range` | Generates a random integer within a specified range. |
| `random_float` | Generates a random floating‑point number. |

### Coordinate Functions

| Function | Description |
| --- | --- |
| `random_latitude` | Generates a random latitude value. |
| `random_longitude` | Generates a random longitude value. |

### Text Functions

| Function | Description |
| --- | --- |
| `concat` | Concatenates multiple fields or values. |
| `initials` | Extracts initials from a name. |
| `lorem_text` | Generates placeholder text (multiple sentences). |
| `lorem_sentence` | Generates a single placeholder sentence. |
| `email_from_name` | Creates an email address based on a name. |
| `random_color` | Generates a random color in hex or RGB format. |

### Hierarchy Functions

| Function | Description |
| --- | --- |
| `inherit_hierarchy` | Inherits hierarchical attributes from a parent record. |
| `hierarchical_name` | Builds a name based on hierarchical structure. |

### Taxonomy Functions

| Function | Description |
| --- | --- |
| `inherit_taxonomy` | Inherits taxonomic information from a parent or source. |
| `assign_valid_taxon` | Assigns a valid taxon based on rules or hierarchy. |

### Sequence Functions

| Function | Description |
| --- | --- |
| `sequence_within_group` | Generates sequential numbers grouped by a field. |

### Polymorphic Functions

| Function | Description |
| --- | --- |
| `random_record_id` | Selects a random record ID from a referenced table. |
| `inherit_or_source` | Inherits a value or retrieves it from a source table. |

### Conversion Functions

| Function | Description |
| --- | --- |
| `convert_beaufort_to_kmh` | Converts Beaufort scale values to km/h. |

## 8. Function Arguments (`args`)

All arguments allowed by the schema:

| Argument | Type |
| --- | --- |
| `fields` | string[] |
| `field` | string |
| `parentField` | string |
| `parentSource` | string |
| `dateField` | string |
| `delimiter` | string |
| `min` | number |
| `max` | number |
| `decimals` | integer |
| `groupField` | string |
| `timeField` | string |
| `textcase` | uppercase, lowercase, titlecase |
| `tableField` | string |
| `nullableProbability` | number (0–1) |
| `skipNulls` | boolean |
| `colorFormat` | hex, rgb |
| `prefix` | string |
| `suffix` | string |
| `rankField` | string |
| `level` | string |
| `lengthField` | string |
| `widthField` | string |

## 9. Best Practices

- Use `dependsOn` whenever a function depends on another field.  
- Use `nullable: true` when the field may be null.  
- Use `optionalParent` when FK may be null.  
- Use `sequence_within_group` for group-based numbering.  
- Validate `tableField` in polymorphic tables.  
- Avoid `additionalProperties` outside allowed sections.  

| Variable | Description |
|:----|:--------------------|
| `iso_o` | Exporter (ISO 3-digit country code) |
| `hs6` | Product category (HS6 product rev. 2022) |
| `year` | Year |
| `export_opt` | Export value of product p by origin o in year t (thousand USD) |
| `c1_X` | C1: HHI of the origin's exports across destinations |
| `c2_X` | C2: HHI of world imports across importers |
| `c3_X` | C3: export to import ratio of the origin (9999 if no imports) |
| `c4_X` | C4: =1 if C1-C3 are met in at least 2 of the years t, t-1, t-2 |
| `first_d` | ISO code of the main destination of product p from origin o in year t |
| `share_X_odpt` | Share (%) of the main destination in origin o's exports of product p |
| `dependent_X` | =1 if c1_X>0.4 & c2_X>0.4 & c3_X>1 in year t and c4_X=1 |

: GeoDep_X variable descriptions {#tbl-geodep-x}

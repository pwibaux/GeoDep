| Variable | Description |
|:----|:--------------------|
| `iso_d` | Importer (ISO 3-digit country code) |
| `hs6` | Product category (HS6 product rev. 2022) |
| `year` | Year |
| `import_dpt` | Import value of product p by destination d in year t (thousand USD) |
| `c1_M` | C1: HHI of the destination's imports across origins |
| `c2_M` | C2: HHI of world exports across exporters |
| `c3_M` | C3: import to export ratio of the destination (9999 if no exports) |
| `c4_M` | C4: =1 if C1-C3 are met in at least 2 of the years t, t-1, t-2 |
| `first_o` | ISO code of the main supplier of product p to destination d in year t |
| `share_M_odpt` | Share (%) of the main supplier in destination d's imports of product p |
| `dependent_M` | =1 if c1_M>0.4 & c2_M>0.4 & c3_M>1 in year t and c4_M=1 |
| `sect_crm` | =1 if in the UNCTAD list of critical raw materials, 0 otherwise |
| `sect_dual_use` | =1 if in the EU list of dual-use goods, 0 otherwise |
| `sect_health` | =1 if in the health products nomenclature, 0 otherwise |
| `sect_energy` | =1 if in the Energy Charter Treaty (ECT) list, 0 otherwise |
| `sect_agrifood` | =1 if in the World Bank agrifood or FAO fertilizer list, 0 otherwise |
| `sect_other` | =1 if in none of the strategic lists, 0 otherwise |

: GeoDep_M variable descriptions {#tbl-geodep-m}

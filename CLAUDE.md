# GeoDep — Trade dependency explorer (Shiny app)

R Shiny app by IFE-CEPII that maps bilateral import and export dependencies (BACI, HS 2022, year 2024).
The intern's functionality is kept. The visual design was modernised to match the IFE brand
(ife.eco) without copying competitor sites such as tradedependencyatlas.com.

Owner: Pauline Wibaux (IFE-CEPII). Working language for the app: English.

## Files

- `app.R`: the whole app in a single file (UI, CSS, JS, server), about 2,300 lines. **The file must be named `app.R`** for deployment.
- `1_Data/geodep_shiny_inputs.rds`: all the input data, loaded at the top of `app.R` with a relative path. Do not use `here::i_am()` or `here()`, because they break on the server.
- **Data pipeline.** First, `update_data.R` copies `geodep_M_hs22.dta` and `geodep_X_hs22.dta` from Dropbox (`DROPBOX_DIR` in `.Renviron`) to `1_Data/geodep_M_hs22.rds` and `geodep_X_hs22.rds`. Then `preparation.R` (formerly `preparation.qmd`) builds `geodep_shiny_inputs.rds`. Run both with the project folder as the working directory.
  - `master.R` runs the four steps in order (`update_data.R`, `preparation.R`, the app locally, `deploy.R`), each switched on or off with a `RUN_*` flag at the top. `RUN_DEPLOY` is `FALSE` by default.
  - Do not rename variables, and keep the double filter on `dependent_M`/`dependent_X` and `c4`: with future data (e.g. 2025), the two may diverge.
- `www/`: optional. The logos are embedded in `app.R` as base64 data URIs (`LOGO_IFE_FULL`, `LOGO_IFE_MARK`), so `www/` is not needed.

## Page structure

`bslib::page_navbar(id = "main_nav")` (Bootstrap 5):

- **Navbar.**
  - Background: a linear gradient from IFE red `#7D0000` to petrol `#21606E`.
  - Left: the white IFE logo, then "GeoDep". On phones, only the logo mark is shown.
  - `nav_spacer()`, then the tabs: Explorer (`value = "atlas"`), Methodology, Publications, Data ↗, ife.eco ↗.
- **Explorer tab.**
  - Hero title: "GeoDep — Trade dependency explorer". Lead, kept on one line: "Who depends on whom, for what? Explore import and export dependencies for every country, product by product."
  - Meta line: "Data: BACI, HS 2022 · Year: 2024". Below it, on a second line in the same small grey style (`.hero-cite`): "Please cite: …" with the GeoDep working paper (forthcoming, marked `#to be modified`). The user rejected a boxed banner as not minimalist enough.
  - **Sidebar** (`title = "View"`; the content has a 14px top padding so "View" lines up with the collapse arrow):
    - Dimension toggle and "Map shows" toggle (`radioGroupButtons`);
    - Sector;
    - "Countries", a bold label at the same level as "Sector" (`.side-group-label`). Below it, a sub-group (`.side-subgroup`, with smaller grey labels, no indent and no vertical rule, so its boxes line up with Sector) holds "Importer (destination)", the ⇅ swap button, "Exporter (origin)" and the treemap slider (conditional);
    - this gives three levels: the VIEW small-caps title, then bold field labels, then the country sub-fields;
    - Reset selection (solid anthracite `.btn-reset`);
    - downloads.
  - **Map card.**
    - Header: "Click a country to see its dependencies, then click a second one to focus on a trading partner." The same sentence is used for imports and exports.
    - Leaflet map, 560px high.
    - Below the map: the partners panel and the table, both conditional.
  - No statistics boxes above the map, and no "how is this measured" link. These were removed on purpose.
  - **Small screens.**
    - Below 992px the sidebar sits above the map and cannot be collapsed. This uses CSS, plus `open = list(desktop = "open", mobile = "always-above")` for phones.
    - Below 900px the map box keeps a 2.1:1 shape instead of 560px.
    - Below 900px the Leaflet legend is hidden, and a copy appears in `#map_legend_below` under the map. It is filled by the `map_legend` custom message.
- **Methodology tab** (a proper page, keep it):
  - heading "How GeoDep identifies trade dependencies";
  - reference: Clémence Huang, Kévin Lefebvre & Pauline Wibaux, "Trade dependencies: GeoDep database", IFE-CEPII Working Paper (forthcoming);
  - four criteria cards;
  - export-dependent products;
  - Sectors and coverage;
  - Reading the map (2 sentences, placed after Sectors);
  - Data and citation;
  - back button.
- **Publications tab.** Heading "Related publications", with no intro sentence. Cards are built with `publication_card()`, newest first. Each card shows the paper's verbatim abstract and a single "Read the full text (PDF)" link (no abstract-page link):
  - La Lettre du CEPII n°452, "Trade Dependencies: While China Is in a Dominant Position, the European Union Is Not Without Advantages", Wibaux, Feb. 2025 (lettre/2025/let452ang.pdf);
  - Policy Brief 2024-47, Lefebvre & Wibaux, Sept. 2024 (pb2024-47.pdf);
  - Policy Brief 2023-41, "EU Strategic Dependencies: A Long View", Vicard & Wibaux, June 2023 (pb2023-41.pdf).

Wording conventions:
- Use "Explorer", not "Atlas" or "map", in the title.
- Do not mention "EU-27" on the main page.
- Sector names are in sentence case: Critical raw materials, Dual use, Health, Agrifood, Energy, Other. The menu also has "All sectors" and "All strategic sectors".

## Brand and theme

- Fonts:
  - body: Arimo (`font_google("Arimo", wght = c(400,500,600,700))`);
  - headings: Merriweather (400 and 700).
- The `IFE` colour list:

  | Name | Colour | Use |
  |---|---|---|
  | ink | `#3A4048` | softened anthracite: titles, main buttons, the first selected country |
  | ink_soft | `#5A6069` | secondary text |
  | paper | `#F6F5F2` | page background |
  | line | `#E3E1DC` | borders |
  | input | `#CFCCC6` | input borders |
  | accent | `#7D0000` | IFE red: links, main download button |
  | accent_dk | `#5C0000` | |
  | chart | `#6B7480` | large filled areas in charts |
  | brand_red / brand_petrol | `#7D0000` / `#21606E` | navbar gradient |

- Other style details:
  - Small-caps side headings (`.side-heading`, `.sidebar-title`).
  - Segmented toggle buttons.
  - DT tables use `style = "bootstrap5"`, with the active page number shown in white on ink.
  - Selection labels in the sidebar: a filled square before the importer, an outlined square before the exporter.

## Map design (settings constants at the top of `app.R`)

- **Projection.** `MAP_PROJECTION <- "equal_earth"`.
  - Equal Earth (EPSG:8857), registered in JavaScript through proj4 (`equal_earth_js`). The inverse projection is clamped so it never returns NaN.
  - `leafletCRS(crsClass = "L.Proj.CRS", code = "EPSG:8857", resolutions = 2^(17:10))`.
  - `MAP_GLOBE_OUTLINE <- TRUE`: the globe outline is drawn instead of a rectangle.
  - Antarctica is removed.
  - `fix_antimeridian()` (shift the negative longitudes, then `st_wrap_dateline`) removes the stray lines across Russia and Fiji. It runs once in `preparation.R`, together with the removal of Antarctica, not in `app.R`.
- **Greys.**

  | Constant | Colour | Meaning |
  |---|---|---|
  | `COL_ZERO` | `#C9C5BD` | 0% dependent |
  | `COL_NODATA` | `#EEECE8` | no data |
  | `COL_OCEAN` | `#F7F8F8` | ocean |
  | `COL_GLOBE` | `#D9D7D2` | globe outline |

- **Palettes** (7-step petrol ramps):
  - imports: `MAP_COLOURS <- c("#DCEBEE","#AED1D7","#83B7C0","#5B9CA8","#3C8090","#266476","#16465A")`;
  - exports, lighter: `MAP_COLOURS_EXPORT <- c("#E4EFF1","#CAE1E5","#B2D3D9","#90BFC7","#71ABB5","#5395A2","#387C8D")`.
  - There is no palette selector for users.
- **Classes.**
  - World view (`map_breaks()`): 7 classes with round limits, chosen from the data.
    - Even steps (0.2, 0.5, 1, 2, 2.5…) by default. A 1-2-5 or 1-3-10 progression is used only when it spreads the countries clearly better, which in practice means export values.
    - The step and the starting point are the ones that minimise the fullest class.
    - The first limit stays at or below the 10th percentile, so China stays in the lightest class for imports. The last limit stays at or above the 75th percentile.
    - Result for the default views:
      - imports, products: 0, 6, 7 … 11, 13;
      - imports, value: 0, 2, 4 … 12, 80;
      - exports, products: 0, 0.2 … 1.2, 1.9;
      - exports, value: 0, 0.03, 0.1, 0.3, 1, 3, 10, 92.
    - History: the user rejected fixed breaks (too pale), adaptive steps (too dark), and quintiles with a split first class (uneven limits with decimals).
  - Partner view (`partner_breaks()`), used once a country is selected: the fixed steps `MAP_PARTNER_STEPS <- c(0.1,0.25,0.5,1,2,5,10,25,50)`. Fixed steps stop China, an outlier, from creating a misleading last class.
- **Selection display.**
  - The first selected country is filled in ink. The second one gets an ink border of weight 2.
  - When the EU is selected, the outlines of its members are drawn with weight 0.8.
  - Hovering any EU member outlines the whole EU (`eu_outline`, `add_eu_hover_outline()`).
  - Outlines are drawn in their own pane (`addMapPane("selection", zIndex = 450)`) so the hover highlight cannot hide them.
  - Name labels use `addLabelOnlyMarkers`, placed at `selection_label_point()` (largest polygon piece, then `st_point_on_surface`; the EU gets c(10, 50)).
- **Zoom.**
  - The zoom control is at the top left.
  - `onRender` fits the view, then calls `setMinZoom(map.getZoom())`, so users cannot zoom out past the opening view.
  - Inside `onRender`, use `this.getMap()`.
  - **Panning is limited to the globe.** `keepInside()` runs on `moveend`. It compares the view's pixel bounds with the globe rectangle (`mapExt`, set in `fitIt()`) and slides back with `panBy`. At the opening zoom it keeps the map centred. Leaflet's `maxBounds` cannot be used here, because the corners of the Equal Earth oval do not map to a lat/lng box.
- **Legend.**
  - World view titles: "Share of products dependent (%)" or "Share of trade value dependent (%)".
  - Partner view title: "Share of X's dependent imports led by each exporter (%)".
  - The labels have no trailing "%" and no "(import)".
- **Tooltips.**
  - World view: "Dependent products: X / Imported (or Exported) products: Y / Share: Z%". In value mode: "Value of dependent imports / Total imports / Share".
  - Partner view: one short line, "Leads X dependent products (Z%)". In value mode it reads "Supplies $X bn (Z%)", or "Absorbs" for exports. The second selected country gets "Selected exporter" (or "importer") above that line. The first selected country shows "Selected country".

## Known pitfalls

- **CSS** is built with `paste0()`, not `sprintf()`, because `sprintf()` has an 8,192-character limit.
- **No double quotes anywhere inside the CSS R string**, not even in CSS comments. A double quote breaks parsing.
- **Do not update input labels with `updateSelectizeInput(label = )`.** It fails with "labelNode must be of length 1" in some Shiny versions. Use the custom message instead: `session$sendCustomMessage("set_label", ...)`. The handler is in the header scripts.
- **Navbar background.** Older bslib versions need strong selectors plus `--bslib-navbar-default-bg`.
- **Polygons are added in two calls:** EU members (className `eu-member`, no highlight), then all other countries (with highlight).
- `iso_name()` must return `as.character`.
- **Sector filter in the charts (imports only).**
  - The treemap is filtered by the selected sector: `sector` argument, with `sector_adjectives` providing the "health products" wording.
  - The sector bar chart keeps all sectors. When a single strategic sector is selected, its name, bar and bar-end label turn IFE red. With two countries, the other sectors' bars are faded instead, because red already marks the leading supplier.
  - The axis labels use `ggtext::element_markdown`. In ggplot2 4 it must be set on `axis.text.y.left`, and bold has to be written as `<b>` in the label.
- **Map PNG download.** The "Download map (PNG)" button sits in the map card footer.
  - `make_map_plot()` redraws the map with ggplot and `geom_sf` in Equal Earth (`coord_sf`): always the whole world, ignoring the zoom. The file is 10 × 6.6 in at 300 dpi.
  - Colours, classes and selection come from the `map_style()` reactive, which the on-screen map also uses. Keep both reading from it.
  - The legend lists every class: `scale_fill_manual(limits =, drop = FALSE)` plus `show.legend = TRUE` on the countries layer, because ggplot2 ≥ 3.5 hides keys for absent values.
  - The title and subtitle are built from the direction, metric, sector and selected countries. The caption carries the source and the "Please cite" text (`#to be modified`).
- **Treemap colours (palette F, chosen by the user).** In order: petrol `#1B7F8E`, IFE red, amber `#F2A33A`, green `#4C9F38`, indigo `#4F5FB8`, teal `#26A6A0`, magenta `#A2479E`, yellow `#E5C230`, navy `#2F4E6E`, coral `#E07A5F`. ROW is grey `#b0b0b0`. Labels are dark (ink) on light boxes (luminance > 0.6) and white otherwise. The user rejected muted palettes as not vivid enough.
- **Chart titles wrap to the plot width.** `make_sector_chart()` and `make_partner_sector_chart()` take a `width_px` argument. `renderPlot` passes it from `session$clientData$output_<id>_width`, and `wrap_title()` uses it to break the lines. PNG downloads pass no width and keep the fixed wrapping. In chart text, use `iso_name_in_sentence()` rather than ISO codes.
- **Area computations on invalid geometries:** avoid `st_area()` under s2, because it crashes. Use the shoelace formula and `st_collection_extract()` instead.

## Testing

- Run locally with `shiny::runApp()` from the project folder.
- Check both directions (imports and exports), the value mode, the EU selection, a two-country selection, a phone width, and the Methodology and Publications tabs.

## Publishing (Posit Connect Cloud)

1. The file must be called `app.R`. A name such as `app_Pauline.R` is not picked up.
2. **Never put `rsconnect::deployApp()` inside `app.R`.** Run it from the console.
3. Delete any `.RData` in the folder. `.Rproj` files are not uploaded anyway.
4. If you get an rlang version error, restart R and update the packages.
5. Deploy from the console:
   ```r
   rsconnect::deployApp(appDir = ".", appPrimaryDoc = "app.R",
                        account = "<connect-cloud account>", server = "connect.posit.cloud")
   ```
   shinyapps.io failed with "signal: killed" (out of memory), so use Connect Cloud.

## Open items

- **Taiwan (S19), on hold.** BACI reports Taiwan as S19, "Other Asia, n.e.s.". The map and the country menu use TWN, so Taiwan shows as "No data", even though it has data and leads 219 dependent products of other countries. Treating S19 as TWN in `preparation.R` would fix it. The user chose to leave it as is for now.

- **Server memory, on hold.** If Connect Cloud runs short of memory, the next saving is to stop keeping both `imports_final` (115 MB) and `traded_by_country` (72 MB), and likewise `exports_final` and `traded_by_country_export`, by making the app work from one table per direction. The user chose to leave them as they are until the server shows a problem.

- Add the link to the GeoDep working paper on the Methodology page once it is published. Search for `#to be modified`.
- The Data ↗ link and the dataset weblink currently point to `bdd_modele_item.asp?id=41`. Check them (`#to be modified`).
- In the download README text, fill in the Release Date, the DOI and the "Reference (Please cite…)" fields (`#to be modified`).

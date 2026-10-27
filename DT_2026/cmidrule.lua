--[[
Traits sous les en-têtes qui regroupent plusieurs colonnes (PDF seulement).

Dans un tableau à deux lignes d'en-tête (ex : "All sectors" au-dessus d'une
colonne, "Strategic sectors" au-dessus de trois), ajoute un trait sous chaque
cellule de la première ligne, sauf celles qui couvrent les deux lignes
("Country"). Les traits sont coupés entre deux groupes (\cmidrule(lr)), ce qui
montre que les colonnes 2 et 3-5 sont deux parties différentes.
En HTML, le même effet est obtenu en CSS (bloc <style> en tête de chaque .qmd).
Le tableau étant réécrit en LaTeX brut, chaque .qmd charge \usepackage{multirow}.

Utilisé par Writing/DT_2026/trade_dependencies_geodep.qmd et
Writing/DT_GeoDep_2026/geodep_database.qmd (filters, at: post-render).
--]]

function Table(tbl)
  if not FORMAT:match("latex") then return nil end
  local rows = tbl.head.rows
  if #rows < 2 then return nil end

  -- Colonnes couvertes par chaque cellule de la première ligne
  local rules = {}
  local col = 1
  for _, cell in ipairs(rows[1].cells) do
    if cell.row_span < 2 then
      table.insert(rules, string.format("\\cmidrule(lr){%d-%d}", col, col + cell.col_span - 1))
    end
    col = col + cell.col_span
  end
  if #rules == 0 then return nil end
  local cmid = table.concat(rules, " ")

  -- Tableau écrit en LaTeX par pandoc, puis trait inséré après la première
  -- ligne d'en-tête (en-tête de la première page et des pages suivantes)
  local latex = pandoc.write(pandoc.Pandoc({tbl}), "latex")
  latex = latex:gsub("(\\toprule\\noalign{}\n.-\\\\)", "%1 " .. cmid:gsub("%%", "%%%%"))
  return pandoc.RawBlock("latex", latex)
end

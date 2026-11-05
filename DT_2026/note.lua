--[[
Source et note sous les figures et les tableaux en plus petit (PDF seulement).

Chaque bloc ::: {.note} est entouré de \begingroup\small ... \endgroup, pour le
distinguer du corps du texte. En HTML, le même effet est obtenu en CSS (classe
.note dans le bloc <style> en tête du .qmd).

Utilisé par Writing/DT_2026/trade_dependencies_geodep.qmd.
--]]

function Div(div)
  if not FORMAT:match("latex") then return nil end
  if not div.classes:includes("note") then return nil end
  div.content:insert(1, pandoc.RawBlock("latex", "\\begingroup\\small"))
  div.content:insert(pandoc.RawBlock("latex", "\\par\\endgroup"))
  return div
end

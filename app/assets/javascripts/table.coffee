Point = window.fudim.point

splitId = (id) ->
  id = id.substring(2)
  i = id.indexOf("-")
  x = id.substring(0, i)
  y = id.substring(i + 1)
  [x, y]

cellContext = (cell) ->
  table = cell.closest("table")
  columnHeader = table.find("thead tr th:nth-child(" + (cell.index() + 1) + ")")
  row = cell.closest("tr")
  rowHeader = row.find("th")
  cell.add(columnHeader).add(rowHeader).add(row).add(table)

pointForCell = (cell) ->
  Point.parse(cellContext(cell).map(() ->
    $(this).data("point")

  ).get())
cellData = (cell, key) ->
  decodeURIComponent(cellContext(cell).filter("[data-" + key + "]").data(key))
domainFor = (cell) ->
  cellData(cell, "domain")
factFor = (cell) ->
  cellData(cell, "fact")

updateDependendValues = (cell) ->
  point = pointForCell(cell)
  table = cell.closest("table")
  table.find("td.sum[data-point]").each(() ->
    sumCell = $(this)
    if (Point.contains(pointForCell(sumCell), point))
      updateCell(sumCell)
  )

updateCell = (cell) ->
  table = cell.closest("table")
  point = pointForCell(cell)
  req = jsRoutes.controllers.Facts.get(domainFor(table), factFor(table), point).ajax({ dataType: "text" })
  req.done((v) ->
    cell.text(v))


# SingleFactTable
$("table#factvalue-table").editableTable((cell, oldValue, newValue, onSuccess, onFail) ->
  point = pointForCell(cell)
  req = jsRoutes.controllers.Facts.save(domainFor(cell), factFor(cell), point).ajax({
    data: newValue
    contentType: "text/plain"
    dataType: "text"
  })
  req.done((v) ->
    onSuccess(v)
    updateDependendValues(cell)
  )
  req.fail(onFail)
)
$("input.summarize").change((event) ->
  window.location.href = $(this).attr("href")
)

# Facts Table
$("table#facts-table").editableTable((cell, oldValue, newValue, onSuccess, onFail) ->
  point = pointForCell(cell)
  req = jsRoutes.controllers.Facts.save(domainFor(cell), factFor(cell), point).ajax({
    data: newValue
    contentType: "text/plain"
    dataType: "text"
  })
  req.done(onSuccess)
  req.fail(onFail)
)

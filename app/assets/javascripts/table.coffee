Point = window.fudim.point

splitId = (id) ->
  id = id.substring(2)
  i = id.indexOf("-")
  x = id.substring(0, i)
  y = id.substring(i+1)
  [x, y]

pointForCell = (cell) ->
  v = if (cell.is("[point]"))
    cell.attr("point")
  else
    table = cell.parents("table")
    [x,y] = splitId(cell.attr("id"))
    pfHolders = [$("#x-"+x), $("#y-"+y), table]
    $.map(pfHolders, (e) -> e.attr("point"))
  Point.parse(v)

updateDependendValues = (cell) ->
  point = pointForCell(cell)
  table = cell.parents("table")
  table.find("td.sum[point]").each(() ->
    sumCell = $(this)
    if (Point.contains(pointForCell(sumCell), point))
      updateCell(sumCell)
  )

updateCell = (cell) ->
  table = cell.parents("table")
  point = pointForCell(cell)
  req = jsRoutes.controllers.Facts.get(factFor(table), point).ajax({ dataType: "text" })
  req.done((v) -> cell.text(v))

factFor = (table) ->
  decodeURIComponent(table.attr("fact"))

$("table#factvalue-table").editableTable((cell, oldValue, newValue, onSuccess, onFail) ->
  table = cell.parents("table")
  point = pointForCell(cell)
  req = jsRoutes.controllers.Facts.save(factFor(table), point).ajax({
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
Point = window.fudim.point

splitId = (id) ->
  id = id.substring(2)
  i = id.indexOf("-")
  x = id.substring(0, i)
  y = id.substring(i+1)
  [x, y]

pointForCell = (cell) ->
  table = cell.parents("table")
  [x,y] = splitId(cell.attr("id"))
  pfHolders = [$("#x-"+x), $("#y-"+y), table]
  Point.parse($.map(pfHolders, (e) -> e.attr("point")))

updateDependendValues = (cell) ->
  point = pointForCell(cell)
  table = cell.parents("table")
  table.find("td.sum[point]").each(() ->
    sumCell = $(this)
    if (Point.contains(Point.parse(sumCell.attr("point")), point))
      updateCell(sumCell)
  )

updateCell = (cell) ->
  console.info("Will update #{cell.text()}") # TODO

$("table#factvalue-table").editableTable((cell, oldValue, newValue, onSuccess, onFail) ->
  table = cell.parents("table")
  fact = decodeURIComponent(table.attr("fact"))

  point = pointForCell(cell)
  req = jsRoutes.controllers.Facts.save(fact, point).ajax({
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
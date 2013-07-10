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
  window.fudim.point.parse($.map(pfHolders, (e) -> e.attr("point")))

$("table#factvalue-table").editableTable((cell, oldValue, newValue, onSuccess, onFail) ->
  table = cell.parents("table")
  fact = decodeURIComponent(table.attr("fact"))

  point = pointForCell(cell)
  req = jsRoutes.controllers.Facts.save(fact, point).ajax({
    data: newValue
    contentType: "text/plain"
    dataType: "text"
  })
  req.done(onSuccess)
  req.fail(onFail)
)

$("input.summarize").change((event) ->
  window.location.href = $(this).attr("href")
)
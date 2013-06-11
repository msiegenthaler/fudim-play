splitId = (id) ->
  id = id.substring(2)
  i = id.indexOf("-")
  x = id.substring(0, i)
  y = id.substring(i+1)
  [x, y]

flashSuccessful = (cell) -> cell

flashFailed = (cell) -> cell
  

$("table#factvalue-table td").editable(
  (value, s) ->
    cell = $(this)
    oldValue = cell.text()
    table = $("#factvalue-table")
    fact = decodeURIComponent(table.attr("fact"))   
    
    [x,y] = splitId(cell.attr("id"))
    pfHolders = [$("#x-"+x), $("#y-"+y), table]
    point = window.fudim.point.parse($.map(pfHolders, (e) -> e.attr("point")))    
    
    req = jsRoutes.controllers.Facts.save(fact, point).ajax({
      data: value
      contentType: "text/plain"
      dataType: "text"
    })
    req.done((d) ->
      cell.text(d)
      flashSuccessful(cell)
    )
    req.fail(->
      cell.text(oldValue)
      flashFailed(cell)
    )
    "Saving.."
  , 
  {
  })
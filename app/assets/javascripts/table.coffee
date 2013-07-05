splitId = (id) ->
  id = id.substring(2)
  i = id.indexOf("-")
  x = id.substring(0, i)
  y = id.substring(i+1)
  [x, y]

flashSuccessful = (cell) ->
  cell.effect("highlight", { color: "#eeffee" })

flashFailed = (cell) ->
  cell.effect("highlight", { color: "#ff8888" })
  

$("table#factvalue-table td.editable").editable(
  (value, s) ->
    cell = $(this)
    oldValue = this.revert
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

$("table#factvalue-table:has(td.editable)").map(() ->
  e = $(this)
  e.addClass("test")
  e.find("thead tr:first-child th:first-child").each(() ->
    $(this).html('<i class="icon-edit"></i>')
  )
)
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
    cell.focus()
    "Saving.."
  , 
  {
    event: "edit",
    select: true,
    onreset: () -> $(this).parent("td").focus()
  })


key = {
  left: 37, up: 38, right: 39, down: 40,
  enter: 13,
  isDisplayable: (k) ->
    48 <= k <= 90 or # numbers and digits
    96 <= k <= 111 or # numpad
    186 <= k <= 192 or
    186 <= k <= 192 or
    219 <= k <= 222
}

$("table#factvalue-table").each(() ->
  table = $(this)
  editIndicator = (table, show) ->
    ei = table.find("thead tr:first-child th:first-child")
    if (show) then ei.html('<i class="icon-edit"></i>')
    else ei.html("")
  updateEditIndicator = () ->
    if ($(document.activeElement).hasClass("editable")) then editIndicator(table, true)
    else editIndicator(table, false)
  table.focusin(updateEditIndicator)
  table.focusout(updateEditIndicator)

  table.keydown((event) ->
    cell = $(event.target)
    switch (event.which)
      when key.right
        cell.next("td.editable").focus()
        event.preventDefault()
      when key.left
        cell.prev("td.editable").focus()
        event.preventDefault()
      when key.up
        cell.parent().prev("tr").children().eq(cell.index()).filter("td.editable").focus()
        event.preventDefault()
      when key.down
        cell.parent().next("tr").children().eq(cell.index()).filter("td.editable").focus()
        event.preventDefault()
      else
        if (key.isDisplayable(event.which)) then cell.trigger("edit")
  )
)
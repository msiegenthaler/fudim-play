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
    onsubmit: () -> cellState($(this).parent("td"), false),
    onerror: () -> cellState($(this).parent("td"), false),
    onreset: () ->
      cell = $(this).parent("td")
      cellState(cell, false)
      cell.focus()
    onblur: "cancel",
    height: "100%",
    width: "100%",
    placeholder: ""
  })

cellState = (cell, edit) ->
  if (edit)
    cell.parent("table").find("td.inEdit").each(() -> cellState($(this), false))
    cell.css("width", cell.width()+"px")
    cell.css("height", cell.height()+"px")
    cell.addClass("inEdit")
  else
    cell.css("width", "")
    cell.css("height", "")
    cell.removeClass("inEdit")


key = {
  left: 37, up: 38, right: 39, down: 40,
  enter: 13, tab: 9,
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
    if ($(document.activeElement).closest("td").hasClass("editable")) then editIndicator(table, true)
    else editIndicator(table, false)
  table.focusin(updateEditIndicator)
  table.focusout(updateEditIndicator)

  edit = (cell) ->
    cellState(cell, true)
    cell.trigger("edit")
  table.keydown((event) ->
    cell = $(event.target).closest("td")
    editDone = () -> if (cell.hasClass("inEdit")) then cell.find("form").submit()
    switch (event.which)
      when key.right
        editDone()
        cell.next("td.editable").focus()
        event.preventDefault()
      when key.left
        editDone()
        cell.prev("td.editable").focus()
        event.preventDefault()
      when key.up
        editDone()
        cell.parent().prev("tr").children().eq(cell.index()).filter("td.editable").focus()
        event.preventDefault()
      when key.down
        editDone()
        cell.parent().next("tr").children().eq(cell.index()).filter("td.editable").focus()
        event.preventDefault()
      when key.tab
        editDone()
      else
        if (key.isDisplayable(event.which))
          if (!cell.hasClass("inEdit")) then edit(cell)
  )
)
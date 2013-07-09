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

selectAllContent = (ofElement) -> ofElement.each(() ->
  selection = window.getSelection()
  selection.removeAllRanges()
  range = document.createRange()
  range.selectNodeContents(this)
  selection.addRange(range)
)

$("table#factvalue-table").each(() ->
  table = $(this)

  table.find("thead tr:first-child th:first-child").html('<i class="icon-edit edit-indicator"></i>')
  editIndicator = (show) ->
    if (show) then table.addClass("can-edit-cell")
    else table.removeClass("can-edit-cell")

  flashSuccessful = (cell) -> cell.effect("highlight", { color: "#eeffee" })
  flashFailed = (cell) -> cell.effect("highlight", { color: "#ff8888" })
  splitId = (id) ->
    id = id.substring(2)
    i = id.indexOf("-")
    x = id.substring(0, i)
    y = id.substring(i+1)
    [x, y]
  save = (cell, oldValue, value) ->
    table = cell.parents("table")
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

  editStart = (cell) ->
    cell.addClass("in-edit")
    cell.attr("value-before", cell.text())
    setTimeout((() -> selectAllContent(cell)), 10)

  editDone = (cell) ->
    if (cell.hasClass("in-edit"))
      newValue = cell.text()
      oldValue = cell.attr("value-before") || ""
      if (oldValue != newValue)
        save(cell, oldValue, newValue)
      cell.removeClass("in-edit")
      cell.removeAttr("value-before")
      window.getSelection().removeAllRanges()

  table.focusin((event) ->
    cell = $(event.target).closest("td[contentEditable]")
    editStart(cell)
    editIndicator(cell.length)
  )
  table.focusout((event) ->
    old = $(event.target).closest("td[contentEditable]")
    if (old.length) then editDone(old)
    editIndicator($(event.relatedTarget).closest("td[contentEditable]").length and
                  $.contains(table.get(0), event.relatedTarget))
  )

  changeSelection = (from, to) ->
    if (to.length)
      editDone(from)
      to.focus()
  table.keydown((event) ->
    cell = $(event.target).closest("td")
    switch (event.which)
      when key.right
        changeSelection(cell, cell.next("td[contentEditable]"))
        event.preventDefault()
      when key.left
        changeSelection(cell, cell.prev("td[contentEditable]"))
        event.preventDefault()
      when key.up
        changeSelection(cell, cell.parent().prev("tr").children().eq(cell.index()).filter("td[contentEditable]"))
        event.preventDefault()
      when key.down, key.enter
        changeSelection(cell, cell.parent().next("tr").children().eq(cell.index()).filter("td[contentEditable]"))
        event.preventDefault()
      when key.tab
        editDone(cell)
  )
)
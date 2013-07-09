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

  editStart = (cell) ->
    setTimeout((() -> selectAllContent(cell)), 10)

  editDone = (cell) ->
    console.log("Changed value of "+cell.attr("id")+" to "+cell.text())
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

  table.keydown((event) ->
    cell = $(event.target).closest("td")
    switch (event.which)
      when key.right
        editDone(cell)
        cell.next("td[contentEditable]").focus()
        event.preventDefault()
      when key.left
        editDone(cell)
        cell.prev("td[contentEditable]").focus()
        event.preventDefault()
      when key.up
        editDone(cell)
        cell.parent().prev("tr").children().eq(cell.index()).filter("td[contentEditable]").focus()
        event.preventDefault()
      when key.down, key.enter
        editDone(cell)
        cell.parent().next("tr").children().eq(cell.index()).filter("td[contentEditable]").focus()
        event.preventDefault()
      when key.tab
        editDone(cell)
  )
)
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

editIndicator = (table, show) ->
  ei = table.find("thead tr:first-child th:first-child")
  if (show) then ei.html('<i class="icon-edit"></i>')
  else ei.html("")

$("table#factvalue-table").each(() ->
  table = $(this)

  updateEditIndicator = () ->
    cell = $(document.activeElement).closest("td")
    editIndicator(table, cell.prop("contentEditable"))
  table.focusin((event) ->
    cell = $(event.target).closest("td[contentEditable]")
    setTimeout((() -> selectAllContent(cell)), 10)
    updateEditIndicator()
  )
  table.focusout(updateEditIndicator)

  table.keydown((event) ->
    cell = $(event.target).closest("td")
    editDone = () -> true
    switch (event.which)
      when key.right
        editDone()
        cell.next("td[contentEditable]").focus()
        event.preventDefault()
      when key.left
        editDone()
        cell.prev("td[contentEditable]").focus()
        event.preventDefault()
      when key.up
        editDone()
        cell.parent().prev("tr").children().eq(cell.index()).filter("td[contentEditable]").focus()
        event.preventDefault()
      when key.down, key.enter
        editDone()
        cell.parent().next("tr").children().eq(cell.index()).filter("td[contentEditable]").focus()
        event.preventDefault()
      when key.tab
        editDone()
        event.preventDefault()
  )
)
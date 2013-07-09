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

jQuery.fn.extend({
  ###
  A table that supports editing its tds that have the attribute 'contentEditable' on it.
  When the user leaves the cell its content is passed to the save function that i.e. submits
  it to the server using an ajax request.
  Usage: 
    saveFun = (cell, oldValue, newValue, onSuccess, onFail) ->
      req = #new Ajax request
      req.done((value) -> onSuccess(value))
      req.fail(() -> onFail())
    $("#mytable").editableTable(saveFun)
  ###
  editableTable: (save) ->
    table = $(this)

    editIndicator = (show) ->
      if (show) then table.addClass("can-edit-cell")
      else table.removeClass("can-edit-cell")

    editStart = (cell) ->
      cell.addClass("in-edit")
      cell.attr("value-before", cell.text())
      selAll = () ->
        selected = $(document.activeElement).closest("td")
        if (selected.length and selected.get(0) == cell.get(0))
          selectAllContent(cell)
      setTimeout(selAll, 10)

    flashSuccessful = (cell) -> cell.effect("highlight", { color: "#eeffee" })
    flashFailed = (cell) -> cell.effect("highlight", { color: "#ff8888" })
    doSave = (cell, resetText = true) -> if (cell.hasClass("in-edit"))
      newValue = cell.text()
      oldValue = cell.attr("value-before") || ""
      if (oldValue != newValue)
        onSuccess = (value) ->
            if (resetText) then cell.text(value)
            flashSuccessful(cell)
        onFail = () ->
            if (resetText) then cell.text(oldValue)
            flashFailed(cell)
        save(cell, oldValue, newValue, onSuccess, onFail)
    editDone = (cell) -> if (cell.hasClass("in-edit"))
      doSave(cell)
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
      else doSave(from, false)
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
})
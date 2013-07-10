window.fudim = {}

# Parse what JsHelper.asUrlPart produces.
parse = (x) ->
  x = [x] unless x instanceof Array
  ps = $.map(x, (e) -> decodeURIComponent(e))
  p = {}
  $.map(ps, (e) -> $.extend(p, $.parseJSON(e)))
  p

# Bindable for Point
unbindPoint = (key, point) ->
  parts = $.map(point, (v, k) -> key+"."+encodeURIComponent(k) + "=" + encodeURIComponent(v))
  parts.join("&")

# True if a contains b, false otherwise
containsPoint = (a, b) ->
  diff = $.map(a, (v,k) -> if (b[k] == v) then [] else [k])
  not diff.length

# Exports
window.fudim.point =
  parse: parse
  unbind: unbindPoint
  contains: containsPoint
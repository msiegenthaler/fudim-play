window.fudim = {}

# Parse what JsHelper.asUrlPart produces.
parse = (x) ->
  ps = $.map(x, (e) -> decodeURIComponent(e))
  p = {}
  $.map(ps, (e) -> $.extend(p, $.parseJSON(e)))
  p

# Bindable for Point
unbindPoint = (key, point) ->
  parts = $.map(point, (v, k) -> key+"."+encodeURIComponent(k) + "=" + encodeURIComponent(v))
  parts.join("&")

# Exports
window.fudim.point =
  parse: parse
  unbind: unbindPoint


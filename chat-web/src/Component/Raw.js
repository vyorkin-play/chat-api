// use at your own risk!
exports.unsafeSetInnerHTML = function(el) {
  return function(html) {
    return function() {
      el.innerHTML = html
    }
  }
}

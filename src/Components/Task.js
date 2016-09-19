'use strict';

// mock document for testing
var document = document;

exports.focusTask = function(id) {
  return function() {
    if (!document) return;
    var $el = document.getElementById('todo-' + id);
    if (document.activeNode !== $el) {
      $el.focus();
    }
  };
};

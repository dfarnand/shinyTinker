(function () {
  var triStateBinding = new Shiny.InputBinding();

  $.extend(triStateBinding, {
    find: function (scope) {
      return $(scope).find("input.tri-state-checkbox");
    },

    getValue: function (el) {
      // Return the current state (0 = neutral, 1 = include, -1 = exclude)
      return parseInt($(el).attr("data-state"));
    },

    setValue: function (el, value) {
      $(el).attr("data-state", value);
      this._updateDisplay(el);
    },

    receiveMessage: function (el, data) {
      if (data.hasOwnProperty("value")) {
        this.setValue(el, data.value);
        // Notify the subscribe callback so input$id updates without a click
        $(el).trigger("triStateChanged");
      }

      if (data.hasOwnProperty("label")) {
        $(el).siblings(".tri-state-label").text(data.label);
      }
    },

    subscribe: function (el, callback) {
      // The wrapping <label> re-dispatches label clicks onto the input, and
      // Space on a focused checkbox fires a click too, so one handler covers
      // mouse, label, and keyboard.
      $(el).on("click.triStateBinding", function () {
        // Cycle through states: 0 -> 1 -> -1 -> 0. data-state is the source
        // of truth; never read el.checked (the browser flips it pre-handler).
        var currentState = parseInt($(el).attr("data-state"));
        var newState = ((currentState + 2) % 3) - 1;

        $(el).attr("data-state", newState);
        triStateBinding._updateDisplay(el);

        callback();
      });

      $(el).on("triStateChanged.triStateBinding", function () {
        callback();
      });
    },

    unsubscribe: function (el) {
      $(el).off(".triStateBinding");
    },

    _updateDisplay: function (el) {
      var state = parseInt($(el).attr("data-state"));

      // Remove all state classes; the mark is drawn by CSS
      $(el).removeClass(
        "tri-state-neutral tri-state-include tri-state-exclude"
      );

      // Sync the native checked/indeterminate properties so assistive tech
      // announces checked (include) / mixed (neutral) / not checked (exclude)
      if (state === 0) {
        $(el).addClass("tri-state-neutral");
        el.checked = false;
        el.indeterminate = true;
      } else if (state === 1) {
        $(el).addClass("tri-state-include");
        el.checked = true;
        el.indeterminate = false;
      } else if (state === -1) {
        $(el).addClass("tri-state-exclude");
        el.checked = false;
        el.indeterminate = false;
      }
    },

    initialize: function (el) {
      this._updateDisplay(el);
    },
  });

  // Priority 10: Shiny's built-in checkbox binding finds input[type=checkbox]
  // at priority 0; higher priority binds first and keeps it off our element
  Shiny.inputBindings.register(triStateBinding, "shinyTinker.triStateCheckbox", 10);
})();

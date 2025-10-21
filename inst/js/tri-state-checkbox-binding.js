(function () {
  var triStateBinding = new Shiny.InputBinding();

  $.extend(triStateBinding, {
    find: function (scope) {
      return $(scope).find(".tri-state-checkbox");
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
      }

      if (data.hasOwnProperty("label")) {
        $(el).parent().find(".tri-state-label").text(data.label);
      }
    },

    subscribe: function (el, callback) {
      $(el).on("click.triStateBinding", function () {
        // Cycle through states: 0 -> 1 -> -1 -> 0
        var currentState = parseInt($(el).attr("data-state"));
        var newState = ((currentState + 2) % 3) - 1;

        $(el).attr("data-state", newState);
        callback();

        triStateBinding._updateDisplay(el);
      });

      // Also allow clicking on the label to toggle
      $(el)
        .parent()
        .find(".tri-state-label")
        .on("click.triStateBinding", function () {
          $(el).trigger("click");
        });
    },

    unsubscribe: function (el) {
      $(el).off(".triStateBinding");
      $(el).parent().find(".tri-state-label").off(".triStateBinding");
    },

    _updateDisplay: function (el) {
      var state = parseInt($(el).attr("data-state"));

      // Remove all state classes
      $(el).removeClass(
        "tri-state-neutral tri-state-include tri-state-exclude"
      );

      // Apply appropriate class based on state
      if (state === 0) {
        $(el).addClass("tri-state-neutral");
        $(el).html("&#x2610;"); // Empty box
      } else if (state === 1) {
        $(el).addClass("tri-state-include");
        $(el).html("&#x2611;"); // Checkmark
      } else if (state === -1) {
        $(el).addClass("tri-state-exclude");
        $(el).html("&#x2612;"); // X mark
      }
    },

    initialize: function (el) {
      this._updateDisplay(el);
    },
  });

  Shiny.inputBindings.register(triStateBinding, "shiny.triStateCheckbox");
})();

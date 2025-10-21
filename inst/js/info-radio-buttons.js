// Shiny input binding for infoRadioButtons
(function () {
  var infoRadioButtonsBinding = new Shiny.InputBinding();

  $.extend(infoRadioButtonsBinding, {
    find: function (scope) {
      return $(scope).find(".shiny-input-radiogroup:has(.info-bubble)");
    },

    initialize: function (el) {
      // Initialize popovers for info bubbles within this input
      initializePopovers(el);
    },

    getValue: function (el) {
      var selected = $(el).find('input[type="radio"]:checked');
      return selected.length > 0 ? selected.val() : null;
    },

    subscribe: function (el, callback) {
      $(el).on("change.infoRadioButtonsBinding", function (e) {
        callback();
      });
    },

    unsubscribe: function (el) {
      $(el).off(".infoRadioButtonsBinding");
    },

    getRatePolicy: function () {
      return {
        policy: "debounce",
        delay: 250,
      };
    },
  });

  Shiny.inputBindings.register(
    infoRadioButtonsBinding,
    "shiny.infoRadioButtons"
  );

  // Function to initialize popovers for a given element
  function initializePopovers(container) {
    var triggers = $(container).find('.info-bubble[data-toggle="popover"]');

    triggers.each(function () {
      var el = this;
      var opts = {
        html: true,
        trigger: "hover focus click",
        placement: "right",
        container: "body",
        boundary: "viewport",
        sanitize: false,
      };

      // Avoid double initialization
      if ($(el).data("popover-initialized")) {
        return;
      }

      if (typeof bootstrap !== "undefined" && bootstrap.Popover) {
        var popover = new bootstrap.Popover(el, opts);
        $(el).on("shown.bs.popover", function () {
          var pop = document.querySelector(".popover.show");
          if (pop) pop.style.maxWidth = "480px";
        });
      } else if (
        typeof $ !== "undefined" &&
        typeof $.fn.popover === "function"
      ) {
        $(el).popover(opts);
        $(el).on("shown.bs.popover", function () {
          var pop = document.querySelector(".popover.show");
          if (pop) pop.style.maxWidth = "480px";
        });
      }

      $(el).data("popover-initialized", true);
    });
  }

  // Global click handler to hide popovers when clicking outside (jQuery path)
  if (typeof $ !== "undefined" && typeof $.fn.popover === "function") {
    $(document).on("click", function (e) {
      if (
        $(e.target).closest('.info-bubble[data-toggle="popover"]').length === 0
      ) {
        $('.info-bubble[data-toggle="popover"]').popover("hide");
      }
    });
  }
})();

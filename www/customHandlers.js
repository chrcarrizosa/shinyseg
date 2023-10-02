// Function to check visibility based on the display CSS property
function isVisible(element) {
  return element.css('display') !== 'none';
}

// Update dropdown badge and number of items
Shiny.addCustomMessageHandler('update-dropdown', function(message) {
  let $visibleItems = $('.dropdown-menu')
    .find('[id^="message"]')
    .filter(function() {
      return isVisible($(this));
    })
    .length;

  if ($visibleItems <= 0) {
    // Hide the badge when count is zero or negative
    $('.nav-item.dropdown').find('.navbar-badge').hide();
    $('#bayes-box').show()
  } else {
    // Show the badge and update its value
    $('.nav-item.dropdown').find('.navbar-badge').html($visibleItems).show();
    $('#bayes-box').hide()
  }

  $('.dropdown-item.dropdown-header').html('You have ' + $visibleItems + ' notifications');
});

// Disable rhandsontable paste using the beforePaste hook)
function disablePaste(hot) {
  hot.addHook('beforePaste', function(data, coords) {
    //alert('Pasting is not allowed in this table.');
    return false;
  });
}

// Disable rhandsontable autofill
function disableFill(hot) {
    hot.updateSettings({
    fillHandle: false
  });
}

// This is a manifest file that'll be compiled into including all the files listed below.
// Add new JavaScript/Coffee code in separate files in this directory and they'll automatically
// be included in the compiled file accessible from http://example.com/assets/application.js
// It's not advisable to add code directly here, but if you do, it'll appear at the bottom of the
// the compiled file.
//

//= require constant
//= require jquery
//= require jquery_ujs
//= require jquery.migrate
//= require jquery.fancybox-1.3.4.pack
//= require jquery.control-key-.1.0
//= require jquery.tooltipster
//= require underscore
//= require pigeon
//= require recurring_select
//= require namespace
//= require global
//= require hub_client
//= require listings
//= require call_flow
//= require_directory .

//= require_directory ./projects
//= require_directory ./custom_bindings
//= require_directory ./devise
//= require_directory ./password
$(function(){
  $('.listing').on('change', 'select', function(){
    var select = $(this);
    var listing = $(this).closest('.listing');
    var key = select.attr('name');
    var value = select.val();
    if (value == '') {
      listing.trigger("listings:filter:key:clear", key);
    } else {
      listing.trigger("listings:filter:key:set", [key, value]);
    }
  }).on('listings:loaded', function(){
    var listing = $(this);
    var filters = listing.data('search').filters;

    $('.filter select', listing).each(function(){
      var select = $(this);
      var key = select.attr('name');
      var value = filters[key];
      if (value) {
        select.val(value);
      } else {
        select.val('');
      }
    });
  });
});
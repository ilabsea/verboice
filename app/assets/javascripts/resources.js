#= require_tree ./resources

onResources(function(){

  window.project = new Project();
  ko.applyBindings(project);

  $(window).bind('beforeunload', function(){
    if(window.project.isResourceEditing()){
      return MSG_QUIT_UNSAVE ;
    }
  });
})

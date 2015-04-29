verboice.ProjectCallLogs = ProjectCallLogs = {
  // Initialization
  initialize: function() {
    var _self = ProjectCallLogs;

    // .recorded-audio events
    $(".recorded-audio").on("play", function() { _self.onAudioPlayed(this); });
    $(".recorded-audio").on("pause", function() { _self.onAudioPaused(this); });
    $(".recorded-audio").on("ended", function() { _self.stopAudio(this); });

    // .audio-control
    $(".audio-control").on("click", function() {
      var audio = $(this).closest("td").find(".recorded-audio")[0];

      audio.paused ? _self.playAudio(audio) : _self.stopAudio(audio);
    });

    // hook fancybox
    $(".fancybox").fancybox({
      modal: true,
      // onClosed callback
      onClosed: function() {
        var audio       = $(this.href).find("audio")[0],
            textarea    = $(this.href).find("textarea");

        _self.stopAudio(audio);
        textarea.val(textarea.data("annotation"));

        // edit annotation button
        var hasAnnotation = textarea.val().length > 0;
        this.orig.toggleClass("fedit", !hasAnnotation);
        this.orig.toggleClass("fedit-blue", hasAnnotation);
      }
    });

    // .save-annotation
    $(".save-annotation").on("click", function() { _self.saveAudioAnnotation(this); });

    // .cancel-edit-annotation
    $(".cancel-edit-annotation").on("click", function() {
      _self.cancelEditAudioAnnotation();
    });

    // download call logs fancybox
    $('.download-call_logs').fancybox();
  },

  // saveAudioAnnotation()
  saveAudioAnnotation: function(button) {
    var _self     = ProjectCallLogs,
        audio     = $(button).siblings("audio"),
        textarea  = $(button).siblings("textarea");

    $.ajax({
      url     : "/call_log_recorded_audios/" + audio.data("audio_id"),
      type    : "PUT",
      data    : {
        call_log_recorded_audio : {
          annotation : textarea.val()
        }
      },
      success : function(data) {
        textarea.data("annotation", data.annotation);
        _self.cancelEditAudioAnnotation();
      }
    });
  },

  // cancelEditAudioAnnotation()
  cancelEditAudioAnnotation: function() {
    $.fancybox.close();
  },

  // stopAudio(audio)
  stopAudio: function(audio) {
    var _self = ProjectCallLogs;

    audio.load();
    audio.pause();
    _self.onAudioPaused(audio);
  },

  // playAudio(audio)
  playAudio: function(audio) {
    var _self = ProjectCallLogs;

    audio.load();
    audio.play();
    _self.onAudioPlayed(audio);
  },

  // onAudioPlayed
  onAudioPlayed: function(audio) {
    var _self   = ProjectCallLogs,
        control = _self.$audioControl(audio);

    control.addClass("fstop");
    control.removeClass("fplay");
  },

  // onAudioPaused
  onAudioPaused: function(audio) {
    var _self   = ProjectCallLogs,
        control = _self.$audioControl(audio);

    control.addClass("fplay");
    control.removeClass("fstop");
  },

  // elements
  $audioControl: function(audio) {
    var audio_id = $(audio).data("audio_id");

    return $("#audio_" + audio_id + " .audio-control");
  }
}

$(function(){
  // TODO: should execute when user in projects/call_logs/index only
  ProjectCallLogs.initialize();
});

#= require workflow/email_resources/email_localized_resource

onWorkflow ->
  class window.EmailUploadLocalizedResource extends EmailLocalizedResource
    AUDIO_UPLOAD = {NO_SELECTED: -1, INVALID: 0, VALID: 1}
    INVALID_AUDIO_UPLOAD_MSG = "Invalid audio file"

    constructor: (hash = {}) ->
      super(hash)

      @label = 'Upload a file'
      @template = 'upload_localized_resource_template'

      @description = ko.observable hash.description
      @has_audio = ko.observable hash.has_uploaded_audio
      @uploaded_status = ko.observable hash.uploaded_status ? AUDIO_UPLOAD.NO_SELECTED
      @filename = ko.observable hash.filename
      @url = ko.computed =>
        if @is_saved()
          "/projects/#{project_id}/resources/#{@parent().id()}/localized_resources/#{@id()}/save_file?filename=#{@filename()}"
        else
          null

      @is_valid = ko.computed =>
        @has_audio()

    to_hash: () =>
      $.extend(super,
        description: @description(),
        filename: @filename()
      )

    type: () =>
      'UploadLocalizedResource'

    download: () =>
      downloadURL "/projects/#{project_id}/resources/#{@parent().id()}/localized_resources/#{@id()}/play_file"

    # fileupload callbacks
    add: (e, data) =>
      if data.files[0].type in ["audio/mp3", "audio/mpeg", "audio/x-wav", "audio/wav"]
        @uploaded_status(AUDIO_UPLOAD.VALID)
        @filename(data.files[0].name)
      else
        @uploaded_status(AUDIO_UPLOAD.INVALID)
        @filename(INVALID_AUDIO_UPLOAD_MSG)
      data.url = @url()

    submit: () =>
      unless @is_saved()
        alert 'Please save this message before uploading a file'
        return false

    done: () =>
      @has_audio(true)

-record(session, {session_id, pbx, flow, stack = [], call_flow, js_context, call_log, address, channel, queued_call, project, contact, default_language, status_callback_url, status_callback_user, status_callback_password, callback_params = []}).
-record(hibernated_session_data, {flow, stack, js_context, channel_id, call_flow, call_log_id, project_id, address, contact_id, status_callback_url, status_callback_user, status_callback_password}).


-record(channel, {id, account_id, call_flow_id, name, config, type, created_at, updated_at}).
-record(call_log, {id, account_id, project_id, finished_at, direction, address,
  state, created_at, updated_at, channel_id, started_at, schedule_id, not_before, call_flow_id, fail_reason}).
-record(call_log_entry, {id, call_id, severity, details, created_at, updated_at}).
-record(call_flow, {id, callback_url, flow, project_id, created_at, updated_at}).
-record(queued_call, {id, channel_id, call_log_id, address, callback_url, flow, call_flow_id, status_callback_url,
  schedule_id, not_before, retries, project_id, time_zone, variables, session_id, created_at, updated_at}).
-record(schedule, {id, name, retries, time_from, time_to, weekdays, project_id, created_at, updated_at}).
-record(project, {id, name, status_callback_url, created_at, updated_at}).
window.onReminderSchedules ?= (callback) -> $(-> callback() if $('#reminder_schedules-main').length > 0)

# Configuration for command line productivity tools for taskwarrior and timewarrior

alias t				"clear && task"
alias tsk			"task"

# show top 5 tasks for each day
alias ttoday		"while true; t list limit:5; sleep 5; end;"
alias ttoday-l	"t due:tom list"

# Show top 5 tasks in general
alias urgent	"while true; t next limit:5; sleep 5; end;"
alias urgent-l	"t due:tom next"

alias tw				"timew"

# Charts
alias burndown	"while true; t burndown.daily; sleep 3; end;"

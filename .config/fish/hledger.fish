set LEDGER_FILE ~/Work/Accounts/Finances.ledger.txt

# Balance Statements
alias bal "hledger bal -f $LEDGER_FILE"

# Income Statements
alias is "hledger is -p thismonth -f $LEDGER_FILE"
alias mis "hledger is -M -f $LEDGER_FILE"
alias yis "hledger is -Y -f $LEDGER_FILE"


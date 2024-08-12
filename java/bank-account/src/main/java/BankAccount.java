class BankAccount {
    private boolean closed = true;
    private int balance;

    void open() throws BankAccountActionInvalidException {
        synchronized (this) {
            if (!closed) {
                throw new BankAccountActionInvalidException("Account already open");
            }
            closed = false;
            balance = 0;
        }
    }

    void close() throws BankAccountActionInvalidException {
        synchronized (this) {
            if (closed) {
                throw new BankAccountActionInvalidException("Account closed");
            }
            closed = true;
        }
    }

    void deposit(int amount) throws BankAccountActionInvalidException {
        synchronized (this) {
            if (closed) {
                throw new BankAccountActionInvalidException("Account closed");
            } else if (amount < 0) {
                throw new BankAccountActionInvalidException("Cannot deposit or withdraw negative amount");
            }
            balance += amount;
        }
    }

    void withdraw(int amount) throws BankAccountActionInvalidException {
        synchronized (this) {
            if (closed) {
                throw new BankAccountActionInvalidException("Account closed");
            } else if (amount < 0) {
                throw new BankAccountActionInvalidException("Cannot deposit or withdraw negative amount");
            } else if (balance == 0) {
                throw new BankAccountActionInvalidException("Cannot withdraw money from an empty account");
            } else if (balance < amount) {
                throw new BankAccountActionInvalidException("Cannot withdraw more money than is currently in the account");
            }
            balance -= amount;
        }
    }

    int getBalance() throws BankAccountActionInvalidException {
        synchronized (this) {
            if (closed) {
                throw new BankAccountActionInvalidException("Account closed");
            }
            return balance;
        }
    }
}

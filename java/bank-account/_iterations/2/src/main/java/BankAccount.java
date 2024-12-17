class BankAccount {
    private static final int CLOSED = -1;

    private int balance = CLOSED;

    void open() throws BankAccountActionInvalidException {
        synchronized (this) {
            validateClosed();
            balance = 0;
        }
    }

    void close() throws BankAccountActionInvalidException {
        synchronized (this) {
            validateOpen();
            balance = CLOSED;
        }
    }

    void deposit(int amount) throws BankAccountActionInvalidException {
        validateAmount(amount);

        synchronized (this) {
            validateNotClosed();
            balance += amount;
        }
    }

    void withdraw(int amount) throws BankAccountActionInvalidException {
        validateAmount(amount);

        synchronized (this) {
            validateNotClosed();
            if (balance == 0) {
                throw new BankAccountActionInvalidException("Cannot withdraw money from an empty account");
            } else if (balance < amount) {
                throw new BankAccountActionInvalidException("Cannot withdraw more money than is currently in the account");
            }
            balance -= amount;
        }
    }

    int getBalance() throws BankAccountActionInvalidException {
        synchronized (this) {
            validateNotClosed();
            return balance;
        }
    }

    private static void validateAmount(int amount) throws BankAccountActionInvalidException {
        if (amount < 0) {
            throw new BankAccountActionInvalidException("Cannot deposit or withdraw negative amount");
        }
    }

    private void validateClosed() throws BankAccountActionInvalidException {
        if (balance != CLOSED) {
            throw new BankAccountActionInvalidException("Account already open");
        }
    }

    private void validateOpen() throws BankAccountActionInvalidException {
        if (balance == CLOSED) {
            throw new BankAccountActionInvalidException("Account not open");
        }
    }

    private void validateNotClosed() throws BankAccountActionInvalidException {
        if (balance == CLOSED) {
            throw new BankAccountActionInvalidException("Account closed");
        }
    }
}

defmodule Newsletter do
  def read_emails(path) do
    File.read!(path) |> String.split()
  end

  def open_log(path) do
    File.open!(path, [:write])
  end

  def log_sent_email(pid, email) do
    IO.puts(pid, email)
  end

  def close_log(pid) do
    File.close(pid)
  end

  def send_newsletter(emails_path, log_path, send_fun) do
    emails = read_emails(emails_path)
    log = open_log(log_path)
    do_send_newsletter(emails, log, send_fun)
    close_log(log)
  end

  defp do_send_newsletter([], _, _), do: nil
  defp do_send_newsletter([email | tail_emails], log, send_fun) do
    if send_fun.(email) == :ok, do: log_sent_email(log, email)
    do_send_newsletter(tail_emails, log, send_fun)
  end
end

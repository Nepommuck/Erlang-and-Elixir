defmodule Pollutiondb.Repo do
  use Ecto.Repo,
    otp_app: :pollutiondb,
    # adapter: Ecto.Adapters.Postgres
    adapter: Ecto.Adapters.SQLite3
end

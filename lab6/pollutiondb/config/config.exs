# import Config

# config :pollutiondb, Pollutiondb.Repo,
#   database: "pollutiondb_repo",
#   username: "user",
#   password: "pass",
#   hostname: "localhost"

import Config
 
config :pollutiondb, 
  ecto_repos: [Pollutiondb.Repo]
  
config :pollutiondb, Pollutiondb.Repo, 
  database: "database/pollutiondb.db"

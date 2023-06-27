install.packages("paws", repos = "https://cloud.R-project.org")

Sys.setenv("AWS_ACCESS_KEY_ID" = "NHB7G4SKCY51TGMP48UW",
           "AWS_SECRET_ACCESS_KEY" = "yHu6qYa0wqAF+iAWnMYRWldTUONXh2Kyz+WcBM9t",
           "AWS_DEFAULT_REGION" = "us-east-1",
           "AWS_SESSION_TOKEN" = "eyJhbGciOiJIUzUxMiIsInR5cCI6IkpXVCJ9.eyJhY2Nlc3NLZXkiOiJOSEI3RzRTS0NZNTFUR01QNDhVVyIsImFsbG93ZWQtb3JpZ2lucyI6WyIqIl0sImF1ZCI6WyJtaW5pby1kYXRhbm9kZSIsIm9ueXhpYSIsImFjY291bnQiXSwiYXV0aF90aW1lIjoxNjg3ODUyODA1LCJhenAiOiJvbnl4aWEiLCJlbWFpbCI6ImNsYWlyZS5zYWxsZUBhZ3JpY3VsdHVyZS5nb3V2LmZyIiwiZW1haWxfdmVyaWZpZWQiOnRydWUsImV4cCI6MTY4NzkzOTIxMSwiZmFtaWx5X25hbWUiOiJTYWxsw6kiLCJnaXZlbl9uYW1lIjoiQ2xhaXJlIiwiZ3JvdXBzIjpbXSwiaWF0IjoxNjg3ODUyODA3LCJpc3MiOiJodHRwczovL2F1dGgubGFiLnNzcGNsb3VkLmZyL2F1dGgvcmVhbG1zL3NzcGNsb3VkIiwianRpIjoiOWMyYzE3NmEtZGIyZi00Mzc3LWJjZmItNDg5NmY5MDQ2Mjc5IiwibG9jYWxlIjoiZnIiLCJuYW1lIjoiQ2xhaXJlIFNhbGzDqSIsIm5vbmNlIjoiMzU3ZWI1ZDEtMzMwMS00YzhmLTg0MzItZjQxODA5NTcyMmIyIiwicG9saWN5Ijoic3Rzb25seSIsInByZWZlcnJlZF91c2VybmFtZSI6ImNsYWlyZXMiLCJyZWFsbV9hY2Nlc3MiOnsicm9sZXMiOlsib2ZmbGluZV9hY2Nlc3MiLCJ1bWFfYXV0aG9yaXphdGlvbiJdfSwicmVzb3VyY2VfYWNjZXNzIjp7ImFjY291bnQiOnsicm9sZXMiOlsibWFuYWdlLWFjY291bnQiLCJtYW5hZ2UtYWNjb3VudC1saW5rcyIsInZpZXctcHJvZmlsZSJdfX0sInNjb3BlIjoib3BlbmlkIHByb2ZpbGUgZ3JvdXBzIGVtYWlsIiwic2Vzc2lvbl9zdGF0ZSI6ImI0Y2FjZTUyLTI2ODAtNGI1MC04ZjA4LWQ2MWIzZTA2ODUzNSIsInNpZCI6ImI0Y2FjZTUyLTI2ODAtNGI1MC04ZjA4LWQ2MWIzZTA2ODUzNSIsInN1YiI6IjcxNTE2ZTc4LWNiNzMtNDAyNC04Y2RhLWU4Njk5OTI1NWUzOSIsInR5cCI6IkJlYXJlciJ9.9vXqxQx6NgP95aPku8KXZMqB5uQFuqTYOZ4jGINAC3XZi5-YyipEq1KB0Zs92JQ3_z1gt1nj602sdDMi7k0BSw",
           "AWS_S3_ENDPOINT"= "minio.lab.sspcloud.fr")

library("paws")
minio <- paws::s3(config = list(
  credentials = list(
    creds = list(
      access_key_id = Sys.getenv("AWS_ACCESS_KEY_ID"),
      secret_access_key = Sys.getenv("AWS_SECRET_ACCESS_KEY"),
      session_token = Sys.getenv("AWS_SESSION_TOKEN")
    )),
  endpoint = paste0("https://", Sys.getenv("AWS_S3_ENDPOINT")),
  region = Sys.getenv("AWS_DEFAULT_REGION")))

minio$list_buckets()
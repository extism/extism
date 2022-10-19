defmodule Extism.MixProject do
  use Mix.Project

  def project do
    [
      app: :extism,
      version: "0.0.1-rc.4",
      elixir: "~> 1.14",
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      aliases: aliases()
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [:logger]
    ]
  end

  defp deps do
    [
      {:rustler, "~> 0.26.0"},
      {:json, "~> 1.4"},
    ]
  end

  defp aliases do
    [
      fmt: [
        "format",
        "cmd cargo fmt --manifest-path native/io/Cargo.toml"
      ]
    ]
  end
end

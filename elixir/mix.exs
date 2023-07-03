defmodule Extism.MixProject do
  use Mix.Project

  def project do
    [
      app: :extism,
      version: "0.4.0",
      elixir: "~> 1.12",
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      package: package(),
      aliases: aliases(),
      docs: docs()
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
      {:rustler, "~> 0.29.1"},
      {:json, "~> 1.4"},
      {:ex_doc, "~> 0.21", only: :dev, runtime: false}
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

  defp package do
    [
      licenses: ["BSD-3-Clause"],
      description: "Extism Host SDK for Elixir and Erlang",
      name: "extism",
      files: ~w(lib native .formatter.exs mix.exs README.md LICENSE),
      links: %{"GitHub" => "https://github.com/extism/extism"}
    ]
  end

  defp docs do
    [
      main: "Extism",
      logo: "./logo.png",
      main: "readme",
      extras: ["README.md"]
    ]
  end
end

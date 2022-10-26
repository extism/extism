defmodule Extism.MixProject do
  use Mix.Project

  def project do
    [
      app: :extism,
      version: "0.0.1-rc.5",
      elixir: "~> 1.14",
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      package: package(),
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
      files: ~w(lib native priv .formatter.exs mix.exs README.md LICENSE),
      links: %{"GitHub" => "https://github.com/extism/extism"}
    ]
  end
end

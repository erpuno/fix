defmodule FIX.Mixfile do
  use Mix.Project

  def project() do
    [
      app: :fix,
      version: "0.6.0",
      description: "FIX Financial Information Exchange",
      package: package(),
      elixir: "~> 1.7",
      deps: deps()
    ]
  end

  def package do
    [
      files: ~w(doc include src mix.exs LICENSE),
      licenses: ["ISC"],
      maintainers: ["Namdak Tonpa"],
      name: :fix,
      links: %{"GitHub" => "https://github.com/enterprizing/fix"}
    ]
  end


  def application() do
    [mod: {:fix, []}]
  end

  def deps() do
    [
      {:ex_doc, "~> 0.11", only: :dev}
    ]
  end
end

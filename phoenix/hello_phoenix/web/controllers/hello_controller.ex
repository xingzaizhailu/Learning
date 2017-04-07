defmodule HelloPhoenix.HelloController do
    use HelloPhoenix.Web, :controller

    def index(conn, _params) do
        render conn, "index.html"
    end

    def show(conn, %{"messenger" => messenger}) do
      render conn, "show.html", messenger: messenger
    end
    # if want to access the full map of params
    # def show(connn,%{"messenger" => messenger} = params) do
    #   ...
    # end
    # map will always be strings. '=' represent pattern match instead of assignment
end

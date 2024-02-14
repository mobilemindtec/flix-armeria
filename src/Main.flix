

mod Armeria {
  import com.linecorp.armeria.common.HttpRequest;
  import com.linecorp.armeria.common.HttpResponse;
  import com.linecorp.armeria.common.HttpStatus;
  import com.linecorp.armeria.common.MediaType;
  import com.linecorp.armeria.common.MediaTypeNames;
  import com.linecorp.armeria.common.QueryParams;
  import com.linecorp.armeria.common.HttpMethod;
  import com.linecorp.armeria.server.AbstractHttpService;
  import com.linecorp.armeria.server.Server;
  import com.linecorp.armeria.server.ServerBuilder;
  import com.linecorp.armeria.server.ServiceRequestContext;
  import com.linecorp.armeria.server.annotation.Consumes;
  import com.linecorp.armeria.server.annotation.Default;
  import com.linecorp.armeria.server.annotation.Get;
  import com.linecorp.armeria.server.annotation.Param;
  import com.linecorp.armeria.server.annotation.Post;
  import com.linecorp.armeria.server.annotation.Produces;
  import com.linecorp.armeria.server.logging.LoggingService;
  import com.linecorp.armeria.server.HttpService;
  import java.util.concurrent.CompletableFuture;
  import java.lang.Object;
  import java.lang.Void;

  enum Method with Eq {
    case Get, Post, Put, Delete, Options, Undefined
  }

  enum State with Eq {
    case Ok, NotFound, ServerError, BadRequest
  }

  pub type alias Request = { method = Method }

  pub type alias Response = { 
    body = String, 
    state = State,
    contentType = String, 
    headers = Map[String, String] }

  pub def responseStr(body: String): Response  =
    {body = body, state = State.Ok, contentType = "text/plain", headers = Map#{}}

  pub def responseStrWithState(state: State, body: String): Response  =
    {body = body, state = state, contentType = "text/plain", headers = Map#{}}

  pub def responseStrWithStateHeaders(state: State, body: String, headers: Map[String, String]): Response  =
    {body = body, state = state, contentType = "text/plain", headers = headers}

  pub type alias Service = { 
    method = Method, 
    path = String, 
    f = (Request) -> Response }
  
  def responseOfString(s: String): HttpResponse \ IO =
    import static com.linecorp.armeria.common.HttpResponse.of(String): HttpResponse \ IO as of_;
    of_(s)

  def responseOfState(state: State): HttpResponse \ IO =
    import static com.linecorp.armeria.common.HttpResponse.of(Int32): HttpResponse \ IO as of_;
    match state {
      case State.Ok => of_(200)
      case State.NotFound => of_(404)
      case State.ServerError => of_(500)
      case _ => of_(404)
    }

  def convertHttpMethod(req: HttpRequest): Method \ IO =
    import com.linecorp.armeria.common.HttpRequest.method(): HttpMethod \ {} as requestMethod;
    import com.linecorp.armeria.common.HttpMethod.name(): String \ IO as enumName;
    match req |> requestMethod |> enumName {
      case "GET" => Method.Get
      case "POST" => Method.Post
      case "PUT" => Method.Put
      case "DELETE" => Method.Delete
      case "OPTIONS" => Method.Options
      case _ => Method.Undefined
    }

  def newServiceSpec(spec: Service): HttpService \ IO = new HttpService {
    def serve(_this: HttpService, _ctx: ServiceRequestContext, req: HttpRequest): HttpResponse \ IO =  
      let m = convertHttpMethod(req);
      if (m == spec.method) {
        let resp = spec.f({method = m});
        responseOfString(resp.body)
      } else responseOfState(State.NotFound)
  }

  def addService(builder: ServerBuilder, service: Service): Unit \ IO =
    import com.linecorp.armeria.server.ServerBuilder.service(String, HttpService): ServerBuilder \ IO as serverService;
    serverService(builder, service.path, newServiceSpec(service));
    ()

  pub def start(port: Int32, services: List[Service]): Unit \ IO =
    import static com.linecorp.armeria.server.Server.builder(): ServerBuilder \ IO as newBuilder;
    import com.linecorp.armeria.server.ServerBuilder.build(): Server \ IO as serverBuild;
    import com.linecorp.armeria.server.ServerBuilder.http(Int32): ServerBuilder \ IO as serverHttp;
    import com.linecorp.armeria.server.Server.start(): CompletableFuture \ IO as serverStart;
    import java.util.concurrent.CompletableFuture.join(): ##java.lang.Object \ IO as join;
    let builder = newBuilder();
    serverHttp(builder, port);
    foreach(s <- services)
      addService(builder, s);      
    //serverService(builder, "/", indexService());
    builder |> serverBuild |> serverStart |> join;
    ()

}

def main(): Unit \ IO = 
  let services: List[Armeria.Service] = 
    { method = Armeria.Method.Get, 
      path = "/", 
      f = (_req) -> Armeria.responseStr("hello!!") } :: Nil;
  Armeria.start(8181, services)
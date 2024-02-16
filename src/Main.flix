

mod Armeria {
  pub enum HttpMethod with Eq {
    case Get, Post, Put, Delete, Options, Head, Patch, Undefined
  }

  pub enum HttpStatus with Eq {
    case Ok, NotFound, ServerError, BadRequest
  }

  pub type alias HttpRequest = { 
    target = String,        
    method = HttpMethod,
    contentType = Option[String], 
    body = Option[String],
    headers = Map[String, String]
  }

  pub type alias HttpResponse = { 
    status = HttpStatus,
    contentType = Option[String], 
    body = Option[String], 
    headers = Map[String, String] }

  pub def ok(body: String, contentType: String): HttpResponse  =
    {body = Some(body), status = HttpStatus.Ok, contentType = Some(contentType), headers = Map#{}}

  pub def badRequest(body: String, contentType: String): HttpResponse  =
    {body = Some(body), status = HttpStatus.BadRequest, contentType = Some(contentType), headers = Map#{}}

  pub def serverError(body: String, contentType: String): HttpResponse  =
    {body = Some(body), status = HttpStatus.ServerError, contentType = Some(contentType), headers = Map#{}}

  pub def notFound(contentType: String): HttpResponse  =
    {body = None, status = HttpStatus.NotFound, contentType = Some(contentType), headers = Map#{}}

  pub type alias Route = { 
    method = HttpMethod, 
    target = String, 
    handle = (HttpRequest) -> HttpResponse }
}

mod ArmeriaApi {
  import com.linecorp.armeria.common.HttpRequest;
  import com.linecorp.armeria.common.HttpResponse;
  import com.linecorp.armeria.common.RequestHeaders;
  import com.linecorp.armeria.common.DefaultRequestHeaders;
  import com.linecorp.armeria.common.HttpStatus;
  import com.linecorp.armeria.common.MediaType;
  import com.linecorp.armeria.common.MediaTypeNames;
  import com.linecorp.armeria.common.QueryParams;
  import com.linecorp.armeria.common.HttpMethod;
  import com.linecorp.armeria.common.HttpHeaderGetters;
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
  import java.lang.Character;
  import java.lang.CharSequence;
  import java.lang.Thread
  import java.lang.Class;

  def responseOfString(s: String): HttpResponse \ IO =
    import static com.linecorp.armeria.common.HttpResponse.of(String): HttpResponse \ IO as of_;
    of_(s)

  def responseOfStatus(status: Armeria.HttpStatus): HttpResponse \ IO =
    import static com.linecorp.armeria.common.HttpResponse.of(Int32): HttpResponse \ IO as of_;
    match status {
      case Armeria.HttpStatus.Ok => of_(200)
      case Armeria.HttpStatus.NotFound => of_(404)
      case Armeria.HttpStatus.ServerError => of_(500)
      case Armeria.HttpStatus.BadRequest => of_(404)
    }

  def convertRequest(req: HttpRequest): Armeria.HttpRequest \ IO =
    import com.linecorp.armeria.common.HttpRequest.method(): HttpMethod \ {} as requestMethod;
    import com.linecorp.armeria.common.HttpRequest.path(): String \ IO as requestPath;
    import com.linecorp.armeria.common.HttpMethod.name(): String \ IO as enumName;
    import com.linecorp.armeria.common.HttpRequest.headers(): RequestHeaders \ IO as requestHeaders;
    import com.linecorp.armeria.common.HttpHeaders.names() : ##java.util.Set \ IO as headersNames;
    import com.linecorp.armeria.common.HttpHeaders.get(##java.lang.CharSequence) : String \ IO as headersGet;    
    import java.lang.Object.getClass(): Class \ IO as getClass;
    import java.lang.Class.getName(): String \ IO as getClassName;

    let method = match req |> requestMethod |> enumName {
      case "GET" => Armeria.HttpMethod.Get
      case "POST" => Armeria.HttpMethod.Post
      case "PUT" => Armeria.HttpMethod.Put
      case "DELETE" => Armeria.HttpMethod.Delete
      case "OPTIONS" => Armeria.HttpMethod.Options
      case "HEAD" => Armeria.HttpMethod.Head
      case "PATCH" => Armeria.HttpMethod.Patch
      case _ => Armeria.HttpMethod.Undefined
    };
    
    let target = req |> requestPath;  
    let reqHeaders = req |> requestHeaders;
    let setOfNames = checked_cast(reqHeaders) |> headersNames;

    println(checked_cast(reqHeaders) |> getClass |> getClassName);

    let names: Set[String] = setOfNames |> Adaptor.fromSet;
    let headers: Map[String, String] = Set.foldRight((name, m) -> Map.insert(name, headersGet(checked_cast(reqHeaders), checked_cast(name)), m), Map#{}, names);  

    let contentType = match Map.find((k, _) -> k == "content-type", headers) {
      case Some((_, v)) => Some(v)
      case _ => None
    };
    {target = target, method = method, contentType = contentType, body = None, headers = headers}

  def newServiceSpec(spec: Armeria.Route): HttpService \ IO = new HttpService {
    def serve(_this: HttpService, _ctx: ServiceRequestContext, req: HttpRequest): HttpResponse \ IO =  

      let request = convertRequest(req);

      match request.method {
        case Armeria.HttpMethod.Undefined =>
          responseOfStatus(Armeria.HttpStatus.ServerError)
        case method =>
          if (method == spec.method) {
            let resp = spec.handle(request);
            match resp.body {
              case Some(body) => responseOfString(body)
              case None => responseOfStatus(resp.status)
            }
          } else responseOfStatus(Armeria.HttpStatus.NotFound)
      }
  }

  def addService(builder: ServerBuilder, service: Armeria.Route): Unit \ IO =
    import com.linecorp.armeria.server.ServerBuilder.service(String, HttpService): ServerBuilder \ IO as serverService;
    serverService(builder, service.target, newServiceSpec(service));
    ()

  def thread_join(): Unit \ IO =
    import static java.lang.Thread.currentThread(): Thread \ IO;
    import java.lang.Thread.join(): Unit \ IO as join;
    let t: Thread = currentThread();
    t |> join;
    ()
    
    


  pub def start(port: Int32, services: List[Armeria.Route]): Unit \ IO =
    import static com.linecorp.armeria.server.Server.builder(): ServerBuilder \ IO as newBuilder;
    import com.linecorp.armeria.server.Server.closeOnJvmShutdown(): Void \ IO as closeOnJvmShutdown;    
    import com.linecorp.armeria.server.ServerBuilder.build(): Server \ IO as serverBuild;
    import com.linecorp.armeria.server.ServerBuilder.http(Int32): ServerBuilder \ IO as serverHttp;
    import com.linecorp.armeria.server.Server.start(): CompletableFuture \ IO as serverStart;
    import java.util.concurrent.CompletableFuture.join(): Object \ IO as join;
    
    let builder = newBuilder();
    serverHttp(builder, port);
    foreach(s <- services)
      addService(builder, s);      
    //serverService(builder, "/", indexService());
    let server = builder |> serverBuild;
    server |> closeOnJvmShutdown;
    server |> serverStart |> join;        
    thread_join()
}

def main(): Unit \ IO = 
  let services: List[Armeria.Route] = 
    { method = Armeria.HttpMethod.Get, 
      target = "/", 
      handle = (_req) -> Armeria.ok("hello!!", "text/plain") } :: Nil;
  ArmeriaApi.start(8181, services)
export const opticEngine = require('./domain.js');

const { contexts, diff } = opticEngine.com.useoptic;

export function setLogger(f: Function) {
  const wrapper = new opticEngine.com.useoptic.LoggerWrapper(f);
  opticEngine.Logger.setLoggerImplementation(wrapper);
}
export const ShapesCommands = contexts.shapes.Commands;
export const ShapesHelper = contexts.shapes.ShapesHelper();
export const RequestsHelper = contexts.requests.RequestsServiceHelper();
export const ContentTypesHelper = contexts.requests.ContentTypes();

export const RfcCommands = contexts.rfc.Commands;
export const RequestsCommands = contexts.requests.Commands;
export const RfcCommandContext = contexts.rfc.RfcCommandContext;
export const ScalaJSHelpers = opticEngine.ScalaJSHelpers;

export const CompareEquality = opticEngine.com.useoptic.CompareEquality();

export const JsonTrailHelper = opticEngine.com.useoptic.JsonTrailHelper();

export const Facade = contexts.rfc.RfcServiceJSFacade();
export const Queries = (eventStore: any, service: any, aggregateId: string) =>
  new opticEngine.Queries(eventStore, service, aggregateId);

export function commandsToJson(commands: any[]) {
  return commands.map((x) =>
    JSON.parse(opticEngine.CommandSerialization.toJsonString(x))
  );
}

export function commandsFromJson(commands: any[]) {
  return opticEngine.CommandSerialization.fromJsonString(
    JSON.stringify(commands)
  );
}

export function commandsToJs(commandSequence: any) {
  return opticEngine.CommandSerialization.toJs(commandSequence);
}

export function commandToJs(command: any) {
  return opticEngine.CommandSerialization.toJs(command);
}

export const JsonHelper = opticEngine.com.useoptic.JsonHelper();

function fromJs(x: any) {
  if (typeof x === 'undefined') {
    return JsonHelper.toNone();
  }
  return JsonHelper.toSome(JsonHelper.fromString(JSON.stringify(x)));
}

export const mapScala = (collection: any) => (handler: any) => {
  return ScalaJSHelpers.toJsArray(collection).map(handler);
};

export const filterScala = (collection: any) => (handler: any) => {
  return ScalaJSHelpers.toJsArray(collection).filter(handler);
};

export const getOrUndefined = (option: any) => {
  return ScalaJSHelpers.getOrUndefined(option);
};

export const getOrUndefinedJson = (option: any) => {
  return ScalaJSHelpers.getOrUndefinedJson(option);
};

export const headOrUndefined = (seq: any) => {
  return ScalaJSHelpers.headOrUndefined(seq);
};

export const everyScala = (collection: any) => (handler: any) => {
  return ScalaJSHelpers.toJsArray(collection).every(handler);
};
export const lengthScala = (collection: any) => {
  return ScalaJSHelpers.length(collection);
};
export const toOption = (undefOr: any) => {
  return ScalaJSHelpers.toOption(undefOr);
};
export const getIndex = (collection: any) => (index: number) => {
  return ScalaJSHelpers.getIndex(collection, index);
};
export const getJson = (j: any) => {
  return ScalaJSHelpers.getJson(j);
};

export const InteractionDiffer = diff.InteractionDiffer;
export const BodyUtilities = opticEngine.com.useoptic.diff.interactions.BodyUtilities();
export const ContentTypeHelpers = opticEngine.com.useoptic.diff.interactions.ContentTypeHelpers();
export const OasProjectionHelper = opticEngine.com.useoptic.OASProjectionHelper();
export const StableHashableWrapper =
  opticEngine.com.useoptic.StableHashableWrapper;

export const DiffManagerFacade = opticEngine.com.useoptic.DiffManagerFacade();
export const DiffPreviewer = opticEngine.com.useoptic.ux.DiffPreviewer;
export const DiffHelpers = opticEngine.com.useoptic.diff.helpers.DiffHelpers();
export const DiffResultHelper = opticEngine.com.useoptic.ux.DiffResultHelper();
export const UrlCounterHelper = opticEngine.com.useoptic.UrlCounterHelper();
export const OpticIds = opticEngine.com.useoptic.OpticIdsJs();
export const ChangeLogFacade = opticEngine.com.useoptic.ChangeLogFacade();

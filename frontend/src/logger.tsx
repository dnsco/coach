type IConsoleLevel = "log" | "error";

export function log<T>(obj: T, level: IConsoleLevel = "log"): T {
  console[level](obj);
  return obj;
}
export const logError = <T extends {}>(obj: T): T => log(obj, "error");

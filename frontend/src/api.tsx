import fetch from "isomorphic-unfetch";
import { log, logError } from "./logger";

export interface IApiPerson {
  name: string;
  activities: IApiActivity[];
}

export interface IApiActivity {
  title: string;
  isDelinquent: boolean;
  events: IApiEvent[];
}

export type IApiEvent = [string, string];

const peopleApi: string = process.env.API_URL!;

export const fetchDelinquents = async (): Promise<IApiPerson[]> => {
  log(["about to fetch: ", peopleApi]);
  return fetch(peopleApi)
    .then(async (res: Response) => {
      const delinquents = await res.json();
      return delinquents as IApiPerson[];
    })
    .catch(logError)
    .catch(() => []);
};

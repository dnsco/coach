import fetch from "isomorphic-unfetch";
import { log, logError } from "./logger";

const apiUrl: string = process.env.API_URL!;

const API_URLS = {
  BASE: "",
  PEOPLE: "/people"
};

function apiFetch<T>(url: keyof typeof API_URLS) {
  const endpoint = apiUrl + API_URLS[url];
  log("fetching endpoint: " + endpoint);
  return fetch(endpoint)
    .then(async (res: Response) => {
      const delinquents = await res.json();
      return delinquents as T;
    })
    .catch(logError)
    .catch(() => []);
}

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

export const fetchDelinquents = (): Promise<IApiPerson[]> => apiFetch("BASE");

export type IApiNewPerson = string[];
export const fetchNewPeople = (): Promise<IApiNewPerson[]> =>
  apiFetch("PEOPLE");

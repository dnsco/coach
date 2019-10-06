import fetch from "isomorphic-unfetch";
import { log, logError } from "./logger";

export interface IDelinquent {
  name: string;
  delinquentActivities: string[];
}

const peopleApi: string = process.env.API_URL!;

export const fetchDelinquents = async (): Promise<IDelinquent[]> => {
  log(["about to fetch: ", peopleApi]);
  return fetch(peopleApi)
    .then(async (res: Response) => {
      const delinquents = await res.json();
      return delinquents as IDelinquent[];
    })
    .catch(logError)
    .catch(() => []);
};

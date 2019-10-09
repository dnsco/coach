import Head from "next/head";
import React, { ReactNode } from "react";
import "../../styles/site.scss";

export function Layout({ children }: { children: ReactNode }) {
  return (
    <div>
      <Head>
        <title>Bish didyou?!?!!</title>
      </Head>
      {children}
    </div>
  );
}

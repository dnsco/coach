import React, { HTMLAttributes } from "react";

export type FComponent<T, E = HTMLDivElement> = React.FC<T & HTMLAttributes<E>>;

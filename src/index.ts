import * as functions from 'firebase-functions';
import { createClient, PlacesNearbyRequest, LatLng, PlaceDetailsResult } from "@google/maps";

const API_KEY = functions.config().maps.key;
const client = createClient({ key: API_KEY });

export const restaurants = functions.region("europe-west1").https.onRequest(async (request, response) => {
    const location = request.query.location;
    const radius: number = Number.isNaN(+request.query.radius) ? 10000 : +request.query.radius;
    const maxprice: number = Number.isNaN(+request.query.maxprice) ? 1 : +request.query.maxprice;
    const minprice: number = Number.isNaN(+request.query.minprice) ? 0 : +request.query.minprice;
    const minrating: number = Number.isNaN(+request.query.minrating) ? 4 : +request.query.minrating;
    const minusers: number = Number.isNaN(+request.query.minusers) ? 10 : +request.query.minusers;
    response.send(await getRestaurants(location, radius, maxprice, minprice, minrating, minusers));
});

function getRestaurants(location: LatLng, radius: number, maxprice: number, minprice: number, minrating: number, minusers: number) {
    const req: PlacesNearbyRequest = {
        location: location ?? [51.556203, -0.087639],
        keyword: "restaurant",
        radius: radius,
        maxprice: maxprice,
        minprice: minprice
    };

    return new Promise<PlaceDetailsResult[]>((resolve, reject) => {
        client.placesNearby(req, (err, res) => {
            if (err) {
                reject(err);
            } else {
                const details: Promise<PlaceDetailsResult[]> = Promise.all(res.json.results.map(p => {
                    return new Promise<PlaceDetailsResult>((resolveFunc, rejectFunc) => {
                        client.place({ placeid: p.place_id }, (error, result) => {
                            if (error) {
                                rejectFunc(error);
                            } else {
                                const detail: PlaceDetailsResult = result.json.result;
                                if (detail.rating > minrating && detail.user_ratings_total > minusers) {
                                    resolveFunc(detail);
                                }

                                resolveFunc(undefined);
                            }
                        });
                    });
                }));
                resolve(details);
            }
        });
    });
}
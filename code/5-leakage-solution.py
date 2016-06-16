# coding: utf-8
__author__ = 'ZFTurbo: https://kaggle.com/zfturbo'
# add dictionary for hotel market
# does automatically what my merging and correction did

# can we use the user's past history?
import datetime
from heapq import nlargest
from operator import itemgetter


def prepare_arrays_match():
    f = open("../data/train.csv", "r")
    f.readline()
    best_hotels_od_mar_ulc = dict()
    best_hotels_od_ulc = dict()
    best_hotels_search_dest = dict()
    best_user_dest = dict()
    popular_hotel_cluster_market = dict()
    popular_hotel_cluster = dict()
    total = 0

    # Calc counts
    while 1:
        line = f.readline().strip()
        total += 1

        if total % 2000000 == 0:
            #print('Done')
            #break
            print('Read {} lines...'.format(total))

        if line == '':
            break

        arr = line.split(",")
        user_location_city = arr[5]
        user_id = arr[7]
        orig_destination_distance = arr[6]
        srch_destination_id = arr[16]
        hotel_country = arr[21]
        hotel_market = arr[22]
        is_booking = float(arr[18])
        hotel_cluster = arr[23]

        if user_location_city != '' and orig_destination_distance != '' and \
        hotel_market != '':
            s0 = (user_location_city, orig_destination_distance, hotel_market)

            if s0 in best_hotels_od_mar_ulc:
                if hotel_cluster in best_hotels_od_mar_ulc[s0]:
                    best_hotels_od_mar_ulc[s0][hotel_cluster] += 1
                else:
                    best_hotels_od_mar_ulc[s0][hotel_cluster] = 1
            else:
                best_hotels_od_mar_ulc[s0] = dict()
                best_hotels_od_mar_ulc[s0][hotel_cluster] = 1
        
        if user_location_city != '' and orig_destination_distance != '':
            s1 = (user_location_city, orig_destination_distance)

            if s1 in best_hotels_od_ulc:
                if hotel_cluster in best_hotels_od_ulc[s1]:
                    best_hotels_od_ulc[s1][hotel_cluster] += 1
                else:
                    best_hotels_od_ulc[s1][hotel_cluster] = 1
            else:
                best_hotels_od_ulc[s1] = dict()
                best_hotels_od_ulc[s1][hotel_cluster] = 1

        if srch_destination_id != '' and hotel_country != '' and hotel_market != '':
            s2 = (srch_destination_id, hotel_country, hotel_market)
            if s2 in best_hotels_search_dest:
                if hotel_cluster in best_hotels_search_dest[s2]:
                    best_hotels_search_dest[s2][hotel_cluster] += is_booking*1 + (1-is_booking)*0.15
                else:
                    best_hotels_search_dest[s2][hotel_cluster] = is_booking*1 + (1-is_booking)*0.15
            else:
                best_hotels_search_dest[s2] = dict()
                best_hotels_search_dest[s2][hotel_cluster] = is_booking*1 + (1-is_booking)*0.15

        if hotel_market != '':
            s4 = hotel_market
            if s4 in popular_hotel_cluster_market:
                if hotel_cluster in popular_hotel_cluster_market[s4]:
                    popular_hotel_cluster_market[s4][hotel_cluster] += 1
                else:
                    popular_hotel_cluster_market[s4][hotel_cluster] = 1
            else:
                popular_hotel_cluster_market[s4] = dict()
                popular_hotel_cluster_market[s4][hotel_cluster] = 1
                
        if hotel_cluster in popular_hotel_cluster:
            popular_hotel_cluster[hotel_cluster] += 1
        else:
            popular_hotel_cluster[hotel_cluster] = 1

        if srch_destination_id != '':
            s5 = (user_id, srch_destination_id)
            if s5 in best_user_dest:
                if hotel_cluster in best_user_dest[s5]:
                    best_user_dest[s5][hotel_cluster] += is_booking*1 + (1-is_booking)*0.15
                else:
                    best_user_dest[s5][hotel_cluster] = is_booking*1 + (1-is_booking)*0.15
            else:
                best_user_dest[s5] = dict()
                best_user_dest[s5][hotel_cluster] = is_booking*1 + (1-is_booking)*0.15
                  
    f.close()
    return best_hotels_od_mar_ulc, best_hotels_od_ulc, best_hotels_search_dest, popular_hotel_cluster_market, popular_hotel_cluster, best_user_dest


def gen_submission(best_hotels_search_dest, best_hotels_od_mar_ulc,
                   best_hotels_od_ulc, popular_hotel_cluster_market, popular_hotel_cluster, best_user_dest):
    now = datetime.datetime.now()
    path = '../subm/forum-leak5-16-5-8.csv'
    out = open(path, "w")
    f = open("../data/test.csv", "r")
    f.readline()
    total = 0
    out.write("id,hotel_cluster\n")
    topclasters = nlargest(5, sorted(popular_hotel_cluster.items()), key=itemgetter(1))

    while 1:
        line = f.readline().strip()
        total += 1

        if total % 100000 == 0:
            print('Write {} lines...'.format(total))

        if line == '':
            break

        arr = line.split(",")
        id = arr[0]
        user_id = arr[8]
        user_location_city = arr[6]
        orig_destination_distance = arr[7]
        srch_destination_id = arr[17]
        hotel_country = arr[20]
        hotel_market = arr[21]

        out.write(str(id) + ',')
        filled = []

        s0 = (user_location_city, orig_destination_distance, hotel_market)
        if s0 in best_hotels_od_mar_ulc:
            d = best_hotels_od_mar_ulc[s0]
            topitems = nlargest(5, sorted(d.items()), key = itemgetter(1))
            for i in range(len(topitems)):
                if topitems[i][0] in filled:
                    continue
                if len(filled) == 5:
                    break
                out.write(' ' + topitems[i][0])
                filled.append(topitems[i][0])
                
        s1 = (user_location_city, orig_destination_distance)
        if s1 in best_hotels_od_ulc:
            d = best_hotels_od_ulc[s1]
            topitems = nlargest(5, sorted(d.items()), key=itemgetter(1))
            for i in range(len(topitems)):
                if topitems[i][0] in filled:
                    continue
                if len(filled) == 5:
                    break
                out.write(' ' + topitems[i][0])
                filled.append(topitems[i][0])

        s2 = (srch_destination_id,hotel_country,hotel_market)
        if s2 in best_hotels_search_dest:
            d = best_hotels_search_dest[s2]
            topitems = nlargest(5, d.items(), key=itemgetter(1))
            for i in range(len(topitems)):
                if topitems[i][0] in filled:
                    continue
                if len(filled) == 5:
                    break
                out.write(' ' + topitems[i][0])
                filled.append(topitems[i][0])

        s5 = (user_id, srch_destination_id)
        if s5 in best_user_dest:
            d = best_user_dest[s5]
            topitem = nlargest(5, d.items(), key = itemgetter(1))
            for i in range(len(topitems)):
                if topitems[i][0] in filled:
                    continue
                if len(filled) == 5:
                    break
                out.write(' ' + topitems[i][0])
                filled.append(topitems[i][0])
        
        s3 = hotel_market
        if s3 in popular_hotel_cluster_market:
            d = popular_hotel_cluster_market[s3]
            topitem = nlargest(5, d.items(), key = itemgetter(1))
            for i in range(len(topitems)):
                if topitems[i][0] in filled:
                    continue
                if len(filled) == 5:
                    break
                out.write(' ' + topitems[i][0])
                filled.append(topitems[i][0])
        
        for i in range(len(topclasters)):
            if topclasters[i][0] in filled:
                continue
            if len(filled) == 5:
                break
            out.write(' ' + topclasters[i][0])
            filled.append(topclasters[i][0])

        out.write("\n")
    out.close()


if __name__ == '__main__' :
    best_hotels_od_mar_ulc, best_hotels_od_ulc, best_hotels_search_dest, popular_hotel_cluster_market, popular_hotel_cluster, best_user_dest  = prepare_arrays_match()
    gen_submission(best_hotels_search_dest, best_hotels_od_mar_ulc,
                   best_hotels_od_ulc, popular_hotel_cluster_market, popular_hotel_cluster, best_user_dest)

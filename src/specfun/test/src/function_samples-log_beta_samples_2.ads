package function_samples.log_beta_samples_2 is
Samples : constant Sample_Array :=
(
(5.0000000000000000e-01, 6.4538521137571081e-02),
(5.9999999999999998e-01, -2.2154226994723580e-01),
(6.9999999999999996e-01, -4.7405789957377620e-01),
(8.0000000000000004e-01, -7.0111535020912197e-01),
(9.0000000000000002e-01, -9.0805692694705142e-01),
(1.0000000000000000e+00, -1.0986122886681096e+00),
(1.1000000000000001e+00, -1.2755024554648569e+00),
(1.2000000000000002e+00, -1.4407825464039605e+00),
(1.3000000000000000e+00, -1.5960486753150844e+00),
(1.3999999999999999e+00, -1.7425692250372837e+00),
(1.5000000000000000e+00, -1.8813716279177424e+00),
(1.6000000000000001e+00, -2.0133017391752901e+00),
(1.7000000000000002e+00, -2.1390656631626879e+00),
(1.8000000000000000e+00, -2.2592599682556718e+00),
(1.9000000000000001e+00, -2.3743939957404789e+00),
(2.0000000000000000e+00, -2.4849066497880004e+00),
(2.1000000000000001e+00, -2.5911792493707937e+00),
(2.2000000000000002e+00, -2.6935455148993284e+00),
(2.2999999999999998e+00, -2.7922994335471101e+00),
(2.4000000000000004e+00, -2.8877015293402861e+00),
(2.5000000000000000e+00, -2.9799839165858519e+00),
(2.6000000000000001e+00, -3.0693544134246045e+00),
(2.7000000000000002e+00, -3.1559999208165297e+00),
(2.8000000000000003e+00, -3.2400892212673993e+00),
(2.9000000000000004e+00, -3.3217753146846647e+00),
(3.0000000000000000e+00, -3.4011973816621550e+00),
(3.1000000000000001e+00, -3.4784824443716964e+00),
(3.2000000000000002e+00, -3.5537467801224403e+00),
(3.3000000000000003e+00, -3.6270971311700833e+00),
(3.4000000000000004e+00, -3.6986317455566149e+00),
(3.5000000000000000e+00, -3.7684412769501212e+00),
(3.6000000000000001e+00, -3.8366095661382720e+00),
(3.7000000000000002e+00, -3.9032143226467508e+00),
(3.8000000000000003e+00, -3.9683277216386150e+00),
(3.9000000000000004e+00, -4.0320169286039098e+00),
(4.0000000000000000e+00, -4.0943445622221013e+00),
(4.0999999999999996e+00, -4.1553691040598615e+00),
(4.2000000000000002e+00, -4.2151452623678054e+00),
(4.3000000000000007e+00, -4.2737242960951356e+00),
(4.4000000000000004e+00, -4.3311543043001262e+00),
(4.5000000000000000e+00, -4.3874804853563454e+00),
(4.6000000000000005e+00, -4.4427453697085868e+00),
(4.7000000000000002e+00, -4.4969890293934931e+00),
(4.7999999999999998e+00, -4.5502492670883345e+00),
(4.9000000000000004e+00, -4.6025617870715241e+00),
(5.0000000000000000e+00, -4.6539603501575222e+00),
(5.1000000000000005e+00, -4.7044769143968708e+00),
(5.2000000000000002e+00, -4.7541417631004919e+00),
(5.3000000000000007e+00, -4.8029836215499655e+00),
(5.4000000000000004e+00, -4.8510297635860340e+00),
(5.5000000000000000e+00, -4.8983061091223350e+00),
(5.6000000000000005e+00, -4.9448373135058263e+00),
(5.7000000000000002e+00, -4.9906468495371143e+00),
(5.8000000000000007e+00, -5.0357570828700373e+00),
(5.9000000000000004e+00, -5.0801893414279169e+00),
(6.0000000000000000e+00, -5.1239639794032596e+00),
(6.1000000000000005e+00, -5.1671004363449811e+00),
(6.2000000000000002e+00, -5.2096172917833155e+00),
(6.3000000000000007e+00, -5.2515323157944405e+00),
(6.4000000000000004e+00, -5.2928625158650728e+00),
(6.5000000000000000e+00, -5.3336241803801823e+00),
(6.6000000000000005e+00, -5.3738329190241831e+00),
(6.7000000000000002e+00, -5.4135037003571469e+00),
(6.8000000000000007e+00, -5.4526508868018233e+00),
(6.9000000000000004e+00, -5.4912882672543395e+00),
(7.0000000000000000e+00, -5.5294290875114243e+00),
(7.1000000000000005e+00, -5.5670860786885221e+00),
(7.2000000000000002e+00, -5.6042714837872634e+00),
(7.3000000000000007e+00, -5.6409970825561588e+00),
(7.4000000000000004e+00, -5.6772742147754052e+00),
(7.5000000000000000e+00, -5.7131138020850827e+00),
(7.6000000000000005e+00, -5.7485263684655941e+00),
(7.7000000000000002e+00, -5.7835220594695631e+00),
(7.8000000000000007e+00, -5.8181106602962860e+00),
(7.9000000000000004e+00, -5.8523016127916687e+00),
(8.0000000000000000e+00, -5.8861040314501594e+00),
(8.1000000000000014e+00, -5.9195267184884663e+00),
(8.1999999999999993e+00, -5.9525781780554787e+00),
(8.3000000000000007e+00, -5.9852666296374064e+00),
(8.4000000000000004e+00, -6.0176000207126084e+00),
(8.5000000000000000e+00, -6.0495860387062965e+00),
(8.5999999999999996e+00, -6.0812321222913308e+00),
(8.7000000000000011e+00, -6.1125454720777856e+00),
(8.8000000000000007e+00, -6.1435330607309133e+00),
(8.9000000000000004e+00, -6.1742016425537916e+00),
(9.0000000000000000e+00, -6.2045577625686903e+00),
(9.0999999999999996e+00, -6.2346077651283629e+00),
(9.2000000000000011e+00, -6.2643578020863249e+00),
(9.3000000000000007e+00, -6.2938138405531507e+00),
(9.4000000000000004e+00, -6.3229816702637898e+00),
(9.5000000000000000e+00, -6.3518669105792345e+00),
(9.5999999999999996e+00, -6.3804750171441817e+00),
(9.7000000000000011e+00, -6.4088112882209636e+00),
(9.8000000000000007e+00, -6.4368808707183724e+00),
(9.9000000000000004e+00, -6.4646887659331824e+00),
(1.0000000000000000e+01, -6.4922398350204720e+00),
(1.0100000000000001e+01, -6.5195388042082563e+00),
(1.0200000000000001e+01, -6.5465902697705332e+00),
(1.0300000000000001e+01, -6.5733987027723177e+00),
(1.0400000000000000e+01, -6.5999684535988212e+00),
(1.0500000000000000e+01, -6.6263037562809934e+00),
(1.0600000000000001e+01, -6.6524087326278281e+00),
(1.0700000000000001e+01, -6.6782873961761684e+00),
(1.0800000000000001e+01, -6.7039436559674250e+00),
(1.0900000000000000e+01, -6.7293813201602610e+00),
(1.1000000000000000e+01, -6.7546040994879579e+00),
(1.1100000000000001e+01, -6.7796156105681433e+00),
(1.1200000000000001e+01, -6.8044193790726375e+00),
(1.1300000000000001e+01, -6.8290188427644267e+00),
(1.1400000000000000e+01, -6.8534173544083608e+00),
(1.1500000000000000e+01, -6.8776181845619035e+00),
(1.1600000000000001e+01, -6.9016245242518153e+00),
(1.1700000000000001e+01, -6.9254394875423948e+00),
(1.1800000000000001e+01, -6.9490661140004057e+00),
(1.1900000000000000e+01, -6.9725073710618091e+00),
(1.2000000000000000e+01, -6.9957661563048497e+00),
(1.2100000000000001e+01, -7.0188452996339841e+00),
(1.2200000000000001e+01, -7.0417475653788060e+00),
(1.2300000000000001e+01, -7.0644756543119982e+00),
(1.2400000000000000e+01, -7.0870322055898711e+00),
(1.2500000000000000e+01, -7.1094197986192214e+00),
(1.2600000000000001e+01, -7.1316409548537862e+00),
(1.2700000000000001e+01, -7.1536981395233674e+00),
(1.2800000000000001e+01, -7.1755937632988562e+00),
(1.2900000000000000e+01, -7.1973301838957440e+00),
(1.3000000000000000e+01, -7.2189097076190585e+00),
(1.3100000000000001e+01, -7.2403345908521572e+00),
(1.3200000000000001e+01, -7.2616070414918390e+00),
(1.3300000000000001e+01, -7.2827292203320191e+00),
(1.3400000000000000e+01, -7.3037032423984591e+00),
(1.3500000000000000e+01, -7.3245311782361675e+00),
(1.3600000000000001e+01, -7.3452150551518471e+00),
(1.3700000000000001e+01, -7.3657568584130928e+00),
(1.3800000000000001e+01, -7.3861585324062027e+00),
(1.3900000000000000e+01, -7.4064219817542956e+00),
(1.4000000000000000e+01, -7.4265490723973109e+00),
(1.4100000000000001e+01, -7.4465416326354799e+00),
(1.4200000000000001e+01, -7.4664014541378485e+00),
(1.4300000000000001e+01, -7.4861302929170286e+00),
(1.4400000000000000e+01, -7.5057298702717432e+00),
(1.4500000000000000e+01, -7.5252018736983217e+00),
(1.4600000000000001e+01, -7.5445479577723376e+00),
(1.4700000000000001e+01, -7.5637697450017285e+00),
(1.4800000000000001e+01, -7.5828688266522590e+00),
(1.4900000000000000e+01, -7.6018467635466749e+00),
(1.5000000000000000e+01, -7.6207050868382673e+00),
(1.5100000000000001e+01, -7.6394452987599699e+00),
(1.5200000000000001e+01, -7.6580688733500395e+00),
(1.5300000000000001e+01, -7.6765772571549000e+00),
(1.5400000000000000e+01, -7.6949718699102654e+00),
(1.5500000000000000e+01, -7.7132541052012584e+00),
(1.5600000000000001e+01, -7.7314253311021552e+00),
(1.5700000000000001e+01, -7.7494868907968240e+00),
(1.5800000000000001e+01, -7.7674401031802347e+00),
(1.5900000000000000e+01, -7.7852862634419715e+00),
(1.6000000000000000e+01, -7.8030266436322187e+00),
(1.6100000000000001e+01, -7.8206624932108753e+00),
(1.6200000000000003e+01, -7.8381950395805546e+00),
(1.6300000000000001e+01, -7.8556254886038772e+00),
(1.6399999999999999e+01, -7.8729550251056288e+00),
(1.6500000000000000e+01, -7.8901848133603387e+00),
(1.6600000000000001e+01, -7.9073159975658136e+00),
(1.6699999999999999e+01, -7.9243497023030898e+00),
(1.6800000000000001e+01, -7.9412870329832117e+00),
(1.6900000000000002e+01, -7.9581290762813914e+00),
(1.7000000000000000e+01, -7.9748769005588755e+00),
(1.7100000000000001e+01, -7.9915315562730385e+00),
(1.7199999999999999e+01, -8.0080940763759578e+00),
(1.7300000000000001e+01, -8.0245654767020014e+00),
(1.7400000000000002e+01, -8.0409467563447521e+00),
(1.7500000000000000e+01, -8.0572388980234990e+00),
(1.7600000000000001e+01, -8.0734428684397770e+00),
(1.7699999999999999e+01, -8.0895596186243210e+00),
(1.7800000000000001e+01, -8.1055900842744890e+00),
(1.7900000000000002e+01, -8.1215351860828164e+00),
(1.8000000000000000e+01, -8.1373958300566471e+00),
(1.8100000000000001e+01, -8.1531729078294433e+00),
(1.8199999999999999e+01, -8.1688672969636968e+00),
(1.8300000000000001e+01, -8.1844798612460039e+00),
(1.8400000000000002e+01, -8.2000114509744506e+00),
(1.8500000000000000e+01, -8.2154629032383966e+00),
(1.8600000000000001e+01, -8.2308350421912024e+00),
(1.8699999999999999e+01, -8.2461286793158521e+00),
(1.8800000000000001e+01, -8.2613446136837183e+00),
(1.8900000000000002e+01, -8.2764836322068547e+00),
(1.9000000000000000e+01, -8.2915465098839150e+00),
(1.9100000000000001e+01, -8.3065340100396838e+00),
(1.9199999999999999e+01, -8.3214468845589096e+00),
(1.9300000000000001e+01, -8.3362858741140116e+00),
(1.9400000000000002e+01, -8.3510517083873026e+00),
(1.9500000000000000e+01, -8.3657451062877328e+00),
(1.9600000000000001e+01, -8.3803667761621838e+00),
(1.9700000000000003e+01, -8.3949174160017250e+00),
(1.9800000000000001e+01, -8.4093977136428606e+00),
(1.9900000000000002e+01, -8.4238083469637175e+00),
(2.0000000000000000e+01, -8.4381499840757854e+00)
);

Extra : constant Float_Type :=3.0000000000000000e+00;
end function_samples.log_beta_samples_2;
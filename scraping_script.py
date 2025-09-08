### Credit and thanks to Mr. IED for the core logic of this scraping script.

#!/usr/bin/env python3
"""
pip install requests
Capture a fresh session-ID in Firefox -> DevTools (see instructions below)
and paste it into URL between …/sessions/ and /commands/ .
Replace function helps to make sure you didn't miss anything.

Important: Do not close the tab in Firefox or click anything on the webpage while the script is running!
"""

import requests, random, string, time
from urllib3.exceptions import InsecureRequestWarning

# ── 0. One reusable session ────────────────────────────────────────────────────
session = requests.Session()
requests.packages.urllib3.disable_warnings(InsecureRequestWarning)

# load the page in Firefox and open Dev Tools by pressing Ctrl+I (doesn't seem to work on Chrome/Brave for some reason)
# Set desired filters (date, syndrome, provinces)
# search for the URL starting with the one below and copy-paste the value between "sessions" and "commands" here

# ── 1. Paste fresh session-ID below ───────────────────────────────────────
RANGE_URL = ("https://dvis3.ddc.moph.go.th/vizql/t/DDC_CENTER_DOE/w/priority_v2/"
             "v/Dashboard14/sessions/FCF39E09DD034AAD9D9317237D409532-1:1/"
             "commands/tabdoc/range-filter")

CATEGORICAL_URL = ("https://dvis3.ddc.moph.go.th/vizql/t/DDC_CENTER_DOE/w/priority_v2/"
                   "v/Dashboard14/sessions/FCF39E09DD034AAD9D9317237D409532-1:1/"
                   "commands/tabdoc/categorical-filter")

# ── 2. Static headers ──────────────────────────────────────────────────────────
random_string = ''.join(random.choices(string.ascii_letters + string.digits, k=8))
headers = {
    "X-Tsi-Active-Tab": "Dashboard%201%20(4)",
    "X-Tableau-Version": "2023.1",
    "X-Requested-With": "XMLHttpRequest",
    "Cookie": "workgroup_session_id=null; tableau_locale=en",
    "User-Agent": "Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:140.0) Gecko/20100101 Firefox/140.0",
    "Accept": "text/javascript",
    "Accept-Language": "en-US,en;q=0.5",
    "Accept-Encoding": "gzip, deflate, br, zstd",
    "Referer": "https://dvis3.ddc.moph.go.th/t/DDC_CENTER_DOE/views/priority_v2/Dashboard14?%3Aembed=y&%3AisGuestRedirectFromVizportal=y",
    "Content-Type": f"multipart/form-data; boundary={random_string}",
    "Connection": "keep-alive"
}

# ── 3. Province filter payload template ────────────────────────────────────────
province_template = (f"--{random_string}\r\n"
                     'Content-Disposition: form-data; name="visualIdPresModel"\r\n\r\n'
                     '{"worksheet":"ตารางลำดับโรค 5 ปี","dashboard":"Dashboard 1 (4)"}\r\n'
                     f"--{random_string}\r\n"
                     'Content-Disposition: form-data; name="membershipTarget"\r\n\r\n'
                     'filter\r\n'
                     f"--{random_string}\r\n"
                     'Content-Disposition: form-data; name="globalFieldName"\r\n\r\n'
                     '[sqlproxy.0ju0gj407vd1ga16bb3k71585m80].[none:chw:nk]\r\n'
                     f"--{random_string}\r\n"
                     'Content-Disposition: form-data; name="filterValues"\r\n\r\n'
                     'PROVINCE_VALUE\r\n'
                     f"--{random_string}\r\n"
                     'Content-Disposition: form-data; name="filterUpdateType"\r\n\r\n'
                     'filter-replace\r\n'
                     f"--{random_string}\r\n"
                     'Content-Disposition: form-data; name="heuristicCommandReinterpretation"\r\n\r\n'
                     'do-not-reinterpret-command\r\n'
                     f"--{random_string}--\r\n")

# ── 4. Week range filter payload template ──────────────────────────────────────
week_template = (f"--{random_string}\r\n"
                 "Content-Disposition: form-data; name=\"worksheet\"\r\n\r\n"
                 "ตารางลำดับโรค 5 ปี\r\n"
                 f"--{random_string}\r\n"
                 "Content-Disposition: form-data; name=\"dashboard\"\r\n\r\n"
                 "Dashboard 1 (4)\r\n"
                 f"--{random_string}\r\n"
                 "Content-Disposition: form-data; name=\"globalFieldName\"\r\n\r\n"
                 "[sqlproxy.0ju0gj407vd1ga16bb3k71585m80].[none:Calculation_1601029715857375235:qk]\r\n"
                 f"--{random_string}\r\n"
                 "Content-Disposition: form-data; name=\"filterRangeMin\"\r\n\r\n"
                 "WEEK_NUMBER\r\n"
                 f"--{random_string}\r\n"
                 "Content-Disposition: form-data; name=\"filterRangeMax\"\r\n\r\n"
                 "WEEK_NUMBER\r\n"
                 f"--{random_string}\r\n"
                 "Content-Disposition: form-data; name=\"included\"\r\n\r\n"
                 "include-range\r\n"
                 f"--{random_string}--\r\n")

# ── 5. Complete thai provinces list ────────────────────────────────────────────
provinces = [
    "กระบี่", "กรุงเทพมหานคร", "กาญจนบุรี", "กาฬสินธุ์", "กำแพงเพชร", "ขอนแก่น",
    "จันทบุรี", "ฉะเชิงเทรา", "ชลบุรี", "ชัยนาท", "ชัยภูมิ", "ชุมพร", "เชียงราย",
    "เชียงใหม่", "ตรัง", "ตราด", "ตาก", "นครนายก", "นครปฐม", "นครพนม", "นครราชสีมา",
    "นครศรีธรรมราช", "นครสวรรค์", "นนทบุรี", "นราธิวาส", "น่าน", "บึงกาฬ", "บุรีรัมย์",
    "ปทุมธานี", "ประจวบคีรีขันธ์", "ปราจีนบุรี", "ปัตตานี", "พระนครศรีอยุธยา", "พะเยา",
    "พังงา", "พัทลุง", "พิจิตร", "พิษณุโลก", "เพชรบุรี", "เพชรบูรณ์", "แพร่", "ภูเก็ต",
    "มหาสารคาม", "มุกดาหาร", "แม่ฮ่องสอน", "ยโสธร", "ยะลา", "ร้อยเอ็ด", "ระนอง",
    "ระยอง", "ราชบุรี", "ลพบุรี", "ลำปาง", "ลำพูน", "เลย", "ศรีสะเกษ", "สกลนคร",
    "สงขลา", "สตูล", "สมุทรปราการ", "สมุทรสงคราม", "สมุทรสาคร", "สระแก้ว", "สระบุรี",
    "สิงห์บุรี", "สุโขทัย", "สุพรรณบุรี", "สุราษฎร์ธานี", "สุรินทร์", "หนองคาย",
    "หนองบัวลำภู", "อ่างทอง", "อำนาจเจริญ", "อุดรธานี", "อุตรดิตถ์", "อุทัยธานี", "อุบลราชธานี"
]

# ── 6. Province name mapping ───────────────────────────────────────────────────
province_mapping = {
    "กระบี่": "Krabi", "กรุงเทพมหานคร": "Bangkok", "กาญจนบุรี": "Kanchanaburi", 
    "กาฬสินธุ์": "Kalasin", "กำแพงเพชร": "Kamphaeng Phet", "ขอนแก่น": "Khon Kaen",
    "จันทบุรี": "Chanthaburi", "ฉะเชิงเทรา": "Chachoengsao", "ชลบุรี": "Chonburi", 
    "ชัยนาท": "Chai Nat", "ชัยภูมิ": "Chaiyaphum", "ชุมพร": "Chumphon", 
    "เชียงราย": "Chiang Rai", "เชียงใหม่": "Chiang Mai", "ตรัง": "Trang", 
    "ตราด": "Trat", "ตาก": "Tak", "นครนายก": "Nakhon Nayok", 
    "นครปฐม": "Nakhon Pathom", "นครพนม": "Nakhon Phanom", "นครราชสีมา": "Nakhon Ratchasima", 
    "นครศรีธรรมราช": "Nakhon Si Thammarat", "นครสวรรค์": "Nakhon Sawan", "นนทบุรี": "Nonthaburi", 
    "นราธิวาส": "Narathiwat", "น่าน": "Nan", "บึงกาฬ": "Bueng Kan", 
    "บุรีรัมย์": "Buriram", "ปทุมธานี": "Pathum Thani", "ประจวบคีรีขันธ์": "Prachuap Khiri Khan", 
    "ปราจีนบุรี": "Prachinburi", "ปัตตานี": "Pattani", "พระนครศรีอยุธยา": "Phra Nakhon Si Ayutthaya", 
    "พะเยา": "Phayao", "พังงา": "Phang Nga", "พัทลุง": "Phatthalung", 
    "พิจิตร": "Phichit", "พิษณุโลก": "Phitsanulok", "เพชรบุรี": "Phetchaburi", 
    "เพชรบูรณ์": "Phetchabun", "แพร่": "Phrae", "ภูเก็ต": "Phuket", 
    "มหาสารคาม": "Maha Sarakham", "มุกดาหาร": "Mukdahan", "แม่ฮ่องสอน": "Mae Hong Son", 
    "ยโสธร": "Yasothon", "ยะลา": "Yala", "ร้อยเอ็ด": "Roi Et", 
    "ระนอง": "Ranong", "ระยอง": "Rayong", "ราชบุรี": "Ratchaburi", 
    "ลพบุรี": "Lopburi", "ลำปาง": "Lampang", "ลำพูน": "Lamphun", 
    "เลย": "Loei", "ศรีสะเกษ": "Si Sa Ket", "สกลนคร": "Sakon Nakhon", 
    "สงขลา": "Songkhla", "สตูล": "Satun", "สมุทรปราการ": "Samut Prakan", 
    "สมุทรสงคราม": "Samut Songkhram", "สมุทรสาคร": "Samut Sakhon", "สระแก้ว": "Sa Kaeo", 
    "สระบุรี": "Saraburi", "สิงห์บุรี": "Sing Buri", "สุโขทัย": "Sukhothai", 
    "สุพรรณบุรี": "Suphan Buri", "สุราษฎร์ธานี": "Surat Thani", "สุรินทร์": "Surin", 
    "หนองคาย": "Nong Khai", "หนองบัวลำภู": "Nong Bua Lam Phu", "อ่างทอง": "Ang Thong", 
    "อำนาจเจริญ": "Amnat Charoen", "อุดรธานี": "Udon Thani", "อุตรดิตถ์": "Uttaradit", 
    "อุทัยธานี": "Uthai Thani", "อุบลราชธานี": "Ubon Ratchathani"
}

# ── 7. urls and payload templates ──────────────────────────────────────────────
PARAMETER_URL = ("https://dvis3.ddc.moph.go.th/vizql/t/DDC_CENTER_DOE/w/priority_v2/"
                 "v/Dashboard14/sessions/FCF39E09DD034AAD9D9317237D409532-1:1/"
                 "commands/tabdoc/set-parameter-value")

# Year parameter template
year_template = (f"--{random_string}\r\n"
                 'Content-Disposition: form-data; name="globalFieldName"\r\n\r\n'
                 '[Parameters].[Parameter 1]\r\n'
                 f"--{random_string}\r\n"
                 'Content-Disposition: form-data; name="valueString"\r\n\r\n'
                 'YEAR_VALUE\r\n'
                 f"--{random_string}\r\n"
                 'Content-Disposition: form-data; name="useUsLocale"\r\n\r\n'
                 'false\r\n'
                 f"--{random_string}--\r\n")

# ── 8. Auto-save function ──────────────────────────────────────────────────────
def save_results(results, failed_requests, suffix=""):
    from datetime import datetime
    import csv
    
    timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
    
    # Main results file
    main_filename = f'thailand_disease_4years_{timestamp}{suffix}.csv'
    with open(main_filename, 'w', newline='', encoding='utf-8') as f:
        writer = csv.DictWriter(f, fieldnames=['year', 'province_thai', 'province_english', 'week', 'value'])
        writer.writeheader()
        writer.writerows(results)
    
    # Failed requests file
    if failed_requests:
        failed_filename = f'failed_requests_{timestamp}{suffix}.csv'
        with open(failed_filename, 'w', newline='', encoding='utf-8') as f:
            writer = csv.DictWriter(f, fieldnames=['year', 'province', 'week', 'error'])
            writer.writeheader()
            writer.writerows(failed_requests)
        
        return main_filename, failed_filename
    
    return main_filename, None

# ── 9. COMPLETE 4-YEAR SCRAPING WITH IMPROVED ERROR HANDLING ───────────────────
# Manual inspection of 2024 data shows abrnomalities, with most years having an order of magnitude lower incidence than 2023/2025.
years = ["2563", "2564", "2565", "2566"]
results = []
failed_requests = []
total_operations = len(years) * len(provinces) * 53
current_operation = 0

print(f"Starting 4-year scraping: {len(years)} years × {len(provinces)} provinces × 53 weeks = {total_operations:,} total operations")

try:
    for year_idx, year in enumerate(years):
        print(f"\n=== PROCESSING YEAR: {year} ({year_idx + 1}/{len(years)}) ===")
        
        # Set year parameter
        year_payload = year_template.replace("YEAR_VALUE", year)
        
        try:
            year_response = session.post(PARAMETER_URL,
                                       data=year_payload.encode("utf-8"), 
                                       headers=headers, 
                                       verify=False)
            
            if year_response.status_code != 200:
                print(f"  Error setting year parameter: {year_response.status_code}")
                continue
                
            time.sleep(1.5)
            print(f"  Year {year} parameter set successfully")
            
            for province_idx, province in enumerate(provinces):
                print(f"  Province: {province} ({province_idx + 1}/{len(provinces)})")
                
                # Set province filter
                province_payload = province_template.replace("PROVINCE_VALUE", f'["{province}"]')
                
                try:
                    province_response = session.post(CATEGORICAL_URL, 
                                                   data=province_payload.encode("utf-8"), 
                                                   headers=headers, 
                                                   verify=False)
                    
                    if province_response.status_code != 200:
                        print(f"    Error setting province filter: {province_response.status_code}")
                        for week in range(1, 54): # Record all weeks as failed for this province
                            failed_requests.append({'year': year, 'province': province, 'week': week, 'error': f'Province filter failed: {province_response.status_code}'})
                            current_operation += 1
                        continue
                        
                    time.sleep(0.7)
                    
                    # Process all weeks for this province/year
                    week_success = 0
                    for week in range(1, 54):
                        current_operation += 1
                        progress = (current_operation / total_operations) * 100
                        
                        week_payload = week_template.replace("WEEK_NUMBER", str(week))
                        
                        try:
                            week_response = session.post(RANGE_URL,
                                                       data=week_payload.encode("utf-8"),
                                                       headers=headers,
                                                       verify=False)
                            
                            if week_response.status_code != 200:
                                failed_requests.append({'year': year, 'province': province, 'week': week, 'error': f'HTTP {week_response.status_code}'})
                                continue
                            
                            resp_json = week_response.json()
                            
                            # Data extraction with error handling
                            data_value = 0 # Default to 0 for missing data
                            
                            try:
                                # Try to get the standard data structure
                                segs = resp_json['vqlCmdResponse']['layoutStatus']['applicationPresModel']['dataDictionary']['dataSegments']
                                
                                for seg in segs.values():
                                    for col in seg['dataColumns']:
                                        if col['dataType'] == "real":
                                            if col['dataValues'] and len(col['dataValues']) > 0:
                                                data_value = col['dataValues'][0]
                                            break
                                    if data_value != 0:
                                        break
                                        
                            except KeyError:
                                # No dataDictionary = no data available, keep data_value = 0
                                pass
                            except Exception as e:
                                # Any other error in data extraction
                                failed_requests.append({'year': year, 'province': province, 'week': week, 'error': f'Data extraction error: {str(e)}'})
                                continue
                            
                            # Store the result (even if 0)
                            results.append({
                                'year': year,
                                'province_thai': province,
                                'province_english': province_mapping[province],
                                'week': week,
                                'value': data_value
                            })
                            
                            week_success += 1
                            
                        except Exception as e:
                            failed_requests.append({'year': year, 'province': province, 'week': week, 'error': f'Request error: {str(e)}'})
                            continue

                        # Timing (alter to be slower if there are scraping errors; that seemed to help)
                        time.sleep(0.4 + random.random()*0.1)
                    
                    print(f"    Completed: {week_success}/53 weeks (Progress: {progress:.1f}%)")
                    
                    # Auto-save checkpoint every 10 provinces
                    if (province_idx + 1) % 10 == 0:
                        main_file, failed_file = save_results(results, failed_requests, "_checkpoint")
                        print(f"    Checkpoint saved: {len(results):,} records")
                    
                except Exception as e:
                    print(f"    Error with province {province}: {e}")
                    # Record all weeks as failed for this province
                    for week in range(1, 54):
                        failed_requests.append({'year': year, 'province': province, 'week': week, 'error': f'Province error: {str(e)}'})
                        current_operation += 1
                    continue
                
                time.sleep(0.3) # Pause between provinces
            
        except Exception as e:
            print(f"  Error processing year {year}: {e}")
            continue
        
        # Save after each year
        main_file, failed_file = save_results(results, failed_requests, f"_year{year}")
        print(f"=== YEAR {year} COMPLETED - 💾 {len(results):,} records saved ===")
        time.sleep(2.0)

except KeyboardInterrupt:
    print("\n⚠️  SCRAPING INTERRUPTED BY USER")
    main_file, failed_file = save_results(results, failed_requests, "_interrupted")
    print(f"💾 Results saved to: {main_file}")

except Exception as e:
    print(f"\n❌ UNEXPECTED ERROR: {e}")
    main_file, failed_file = save_results(results, failed_requests, "_error")
    print(f"💾 Emergency save: {main_file}")

finally:
    # Final save
    main_file, failed_file = save_results(results, failed_requests, "_final")
    
    # Summary statistics
    total_expected = len(years) * len(provinces) * 53
    success_rate = (len(results) / total_expected) * 100 if total_expected > 0 else 0
    
    print(f"\n💾 === SCRAPING COMPLETE ===")
    print(f"💾 Total records: {len(results):,}")
    print(f"💾 Expected records: {total_expected:,}")
    print(f"✅ Success rate: {success_rate:.1f}%")
    print(f"❌ Failed requests: {len(failed_requests):,}")
    print(f"💾 Final data: {main_file}")
    
    if failed_file:
        print(f"⚠️  Failed requests: {failed_file}")
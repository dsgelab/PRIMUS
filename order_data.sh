output_dir="/media/volume/Data_20250430/"
sources=(Data_THL_2698_14.02.00_2023_pk1_kauppanimi Data_THL_2698_14.02.00_2023_pk1_kayntisyy_folk Data_THL_2698_14.02.00_2023_pk1_korjaus2 Data_THL_2698_14.02.00_2023_pk1_uudelleen)
registers=(DVV ETK Kela Reseptikeskus THL Tilastokeskus Valvira)

start=$(date +%s)

# Create output directory and register subfolders
mkdir -p "$output_dir"
for reg in "${registers[@]}"; do
  mkdir -p "$output_dir/$reg"
done

# Copy files from each source/register folder to output/register folder
for src in "${sources[@]}"; do
  [ -d "$src" ] || continue
  for reg in "${registers[@]}"; do
    if [ -d "$src/$reg" ]; then
      cp -a "$src/$reg/." "$output_dir/$reg/"
    fi
  done
done

end=$(date +%s)
echo "Elapsed time: $((end - start)) seconds"
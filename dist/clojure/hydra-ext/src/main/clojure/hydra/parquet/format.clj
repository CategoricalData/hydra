(ns hydra.parquet.format)

(declare hydra_parquet_format_type-variants hydra_parquet_format_field_repetition_type-variants hydra_parquet_format_time_unit-variants hydra_parquet_format_logical_type-variants hydra_parquet_format_encoding-variants hydra_parquet_format_compression_codec-variants hydra_parquet_format_page_type-variants hydra_parquet_format_boundary_order-variants hydra_parquet_format_bloom_filter_algorithm-variants hydra_parquet_format_bloom_filter_hash-variants hydra_parquet_format_bloom_filter_compression-variants hydra_parquet_format_column_crypto_meta_data-variants hydra_parquet_format_column_order-variants hydra_parquet_format_encryption_algorithm-variants)

(def hydra_parquet_format_type-variants (list :boolean :int32 :int64 :float :double :byte_array :fixed_len_byte_array))

(def hydra_parquet_format_field_repetition_type-variants (list :required :optional :repeated))

(defrecord hydra_parquet_format_statistics [null_count distinct_count max_value min_value])
(defn make-hydra_parquet_format_statistics [null_count distinct_count max_value min_value] (->hydra_parquet_format_statistics null_count distinct_count max_value min_value))

(defrecord hydra_parquet_format_decimal_type [scale precision])
(defn make-hydra_parquet_format_decimal_type [scale precision] (->hydra_parquet_format_decimal_type scale precision))

(def hydra_parquet_format_time_unit-variants (list :millis :micros :nanos))

(defrecord hydra_parquet_format_timestamp_type [is_adjusted_to_utc unit])
(defn make-hydra_parquet_format_timestamp_type [is_adjusted_to_utc unit] (->hydra_parquet_format_timestamp_type is_adjusted_to_utc unit))

(defrecord hydra_parquet_format_time_type [is_adjusted_to_utc unit])
(defn make-hydra_parquet_format_time_type [is_adjusted_to_utc unit] (->hydra_parquet_format_time_type is_adjusted_to_utc unit))

(defrecord hydra_parquet_format_int_type [bit_width is_signed])
(defn make-hydra_parquet_format_int_type [bit_width is_signed] (->hydra_parquet_format_int_type bit_width is_signed))

(def hydra_parquet_format_logical_type-variants (list :string :map :list :enum :decimal :date :time :timestamp :integer :unknown :json :bson :uuid))

(defrecord hydra_parquet_format_schema_element [type type_length repetition_type name num_children field_id logical_type])
(defn make-hydra_parquet_format_schema_element [type type_length repetition_type name num_children field_id logical_type] (->hydra_parquet_format_schema_element type type_length repetition_type name num_children field_id logical_type))

(def hydra_parquet_format_encoding-variants (list :plain :rle :bit_packed :delta_binary_packed :delta_length_byte_array :delta_byte_array :rle_dictionary :byte_stream_split))

(def hydra_parquet_format_compression_codec-variants (list :uncompressed :snappy :gzip :lzo :brotli :zstd :lz4_raw))

(def hydra_parquet_format_page_type-variants (list :data_page :index_page :dictionary_page :data_page_v2))

(def hydra_parquet_format_boundary_order-variants (list :unordered :ascending :descending))

(defrecord hydra_parquet_format_data_page_header [num_values encoding definition_level_encoding repetition_level_encoding statistics])
(defn make-hydra_parquet_format_data_page_header [num_values encoding definition_level_encoding repetition_level_encoding statistics] (->hydra_parquet_format_data_page_header num_values encoding definition_level_encoding repetition_level_encoding statistics))

(defrecord hydra_parquet_format_index_page_header [])
(defn make-hydra_parquet_format_index_page_header [] (->hydra_parquet_format_index_page_header))

(defrecord hydra_parquet_format_dictionary_page_header [num_values encoding is_sorted])
(defn make-hydra_parquet_format_dictionary_page_header [num_values encoding is_sorted] (->hydra_parquet_format_dictionary_page_header num_values encoding is_sorted))

(defrecord hydra_parquet_format_data_page_header_v2 [num_values num_nulls num_rows encoding definition_levels_byte_length repetition_levels_byte_length is_compressed statistics])
(defn make-hydra_parquet_format_data_page_header_v2 [num_values num_nulls num_rows encoding definition_levels_byte_length repetition_levels_byte_length is_compressed statistics] (->hydra_parquet_format_data_page_header_v2 num_values num_nulls num_rows encoding definition_levels_byte_length repetition_levels_byte_length is_compressed statistics))

(def hydra_parquet_format_bloom_filter_algorithm-variants (list :block))

(def hydra_parquet_format_bloom_filter_hash-variants (list :xxhash))

(def hydra_parquet_format_bloom_filter_compression-variants (list :uncompressed))

(defrecord hydra_parquet_format_bloom_filter_header [num_bytes algorithm hash compression])
(defn make-hydra_parquet_format_bloom_filter_header [num_bytes algorithm hash compression] (->hydra_parquet_format_bloom_filter_header num_bytes algorithm hash compression))

(defrecord hydra_parquet_format_page_header [type uncompressed_page_size compressed_page_size crc data_page_header index_page_header dictionary_page_header data_page_header_v2])
(defn make-hydra_parquet_format_page_header [type uncompressed_page_size compressed_page_size crc data_page_header index_page_header dictionary_page_header data_page_header_v2] (->hydra_parquet_format_page_header type uncompressed_page_size compressed_page_size crc data_page_header index_page_header dictionary_page_header data_page_header_v2))

(defrecord hydra_parquet_format_key_value [key value])
(defn make-hydra_parquet_format_key_value [key value] (->hydra_parquet_format_key_value key value))

(defrecord hydra_parquet_format_sorting_column [column_idx descending nulls_first])
(defn make-hydra_parquet_format_sorting_column [column_idx descending nulls_first] (->hydra_parquet_format_sorting_column column_idx descending nulls_first))

(defrecord hydra_parquet_format_page_encoding_stats [page_type encoding count])
(defn make-hydra_parquet_format_page_encoding_stats [page_type encoding count] (->hydra_parquet_format_page_encoding_stats page_type encoding count))

(defrecord hydra_parquet_format_column_meta_data [type encodings path_in_schema codec num_values total_uncompressed_size total_compressed_size key_value_metadata data_page_offset index_page_offset dictionary_page_offset statistics encoding_stats bloom_filter_offset])
(defn make-hydra_parquet_format_column_meta_data [type encodings path_in_schema codec num_values total_uncompressed_size total_compressed_size key_value_metadata data_page_offset index_page_offset dictionary_page_offset statistics encoding_stats bloom_filter_offset] (->hydra_parquet_format_column_meta_data type encodings path_in_schema codec num_values total_uncompressed_size total_compressed_size key_value_metadata data_page_offset index_page_offset dictionary_page_offset statistics encoding_stats bloom_filter_offset))

(defrecord hydra_parquet_format_encryption_with_footer_key [])
(defn make-hydra_parquet_format_encryption_with_footer_key [] (->hydra_parquet_format_encryption_with_footer_key))

(defrecord hydra_parquet_format_encryption_with_column_key [path_in_schema key_metadata])
(defn make-hydra_parquet_format_encryption_with_column_key [path_in_schema key_metadata] (->hydra_parquet_format_encryption_with_column_key path_in_schema key_metadata))

(def hydra_parquet_format_column_crypto_meta_data-variants (list :encryption_with_footer_key :encryption_with_column_key))

(defrecord hydra_parquet_format_column_chunk [file_path file_offset meta_data offset_index_offset offset_index_length column_index_offset column_index_length crypto_metadata encrypted_column_metadata])
(defn make-hydra_parquet_format_column_chunk [file_path file_offset meta_data offset_index_offset offset_index_length column_index_offset column_index_length crypto_metadata encrypted_column_metadata] (->hydra_parquet_format_column_chunk file_path file_offset meta_data offset_index_offset offset_index_length column_index_offset column_index_length crypto_metadata encrypted_column_metadata))

(defrecord hydra_parquet_format_row_group [columns total_byte_size num_rows sorting_columns file_offset total_compressed_size ordinal])
(defn make-hydra_parquet_format_row_group [columns total_byte_size num_rows sorting_columns file_offset total_compressed_size ordinal] (->hydra_parquet_format_row_group columns total_byte_size num_rows sorting_columns file_offset total_compressed_size ordinal))

(def hydra_parquet_format_column_order-variants (list :type_order))

(defrecord hydra_parquet_format_page_location [offset compressed_page_size first_row_index])
(defn make-hydra_parquet_format_page_location [offset compressed_page_size first_row_index] (->hydra_parquet_format_page_location offset compressed_page_size first_row_index))

(defrecord hydra_parquet_format_offset_index [page_locations])
(defn make-hydra_parquet_format_offset_index [page_locations] (->hydra_parquet_format_offset_index page_locations))

(defrecord hydra_parquet_format_column_index [null_pages min_values max_values boundary_order null_counts])
(defn make-hydra_parquet_format_column_index [null_pages min_values max_values boundary_order null_counts] (->hydra_parquet_format_column_index null_pages min_values max_values boundary_order null_counts))

(defrecord hydra_parquet_format_aes_gcm_v1 [aad_prefix aad_file_unique supply_aad_prefix])
(defn make-hydra_parquet_format_aes_gcm_v1 [aad_prefix aad_file_unique supply_aad_prefix] (->hydra_parquet_format_aes_gcm_v1 aad_prefix aad_file_unique supply_aad_prefix))

(defrecord hydra_parquet_format_aes_gcm_ctr_v1 [aad_prefix aad_file_unique supply_aad_prefix])
(defn make-hydra_parquet_format_aes_gcm_ctr_v1 [aad_prefix aad_file_unique supply_aad_prefix] (->hydra_parquet_format_aes_gcm_ctr_v1 aad_prefix aad_file_unique supply_aad_prefix))

(def hydra_parquet_format_encryption_algorithm-variants (list :aes_gcm_v1 :aes_gcm_ctr_v1))

(defrecord hydra_parquet_format_file_meta_data [version schema num_rows row_groups key_value_metadata created_by column_orders encryption_algorithm footer_signing_key_metadata])
(defn make-hydra_parquet_format_file_meta_data [version schema num_rows row_groups key_value_metadata created_by column_orders encryption_algorithm footer_signing_key_metadata] (->hydra_parquet_format_file_meta_data version schema num_rows row_groups key_value_metadata created_by column_orders encryption_algorithm footer_signing_key_metadata))

(defrecord hydra_parquet_format_file_crypto_meta_data [encryption_algorithm key_metadata])
(defn make-hydra_parquet_format_file_crypto_meta_data [encryption_algorithm key_metadata] (->hydra_parquet_format_file_crypto_meta_data encryption_algorithm key_metadata))

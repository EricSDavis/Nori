bb_getExons <- function(assembly, chromosome, start, stop){

  tx_db <- eval(parse(text = assembly$TxDb))
  org_db <- eval(parse(text = assembly$OrgDb))


  genes_on_chrom <- suppressMessages(genes(tx_db, filter = list(tx_chrom = chromosome))) ## Still GRanges
  genes_in_range <- genes_on_chrom[genes_on_chrom@ranges@start<=stop&genes_on_chrom@ranges@start+genes_on_chrom@ranges@width>=start]
  txs_wo_symbols <- suppressMessages(select(tx_db, keys = genes_in_range$gene_id, columns = columns(tx_db), keytype = "GENEID"))

  if (assembly$gene.id.column == assembly$display.column){
    txs_w_symbols <- txs_wo_symbols
  } else if (assembly$gene.id.column == "ORF"){
    txs_wo_symbols <- cbind(txs_wo_symbols, "SHORTORF" = sub("-.+", "", txs_wo_symbols$GENEID))
    gene_symbols <- suppressMessages(select(org_db, keys = sub("-.+", "", genes_in_range$gene_id), columns = assembly$display.column, keytype = "ORF"))
    txs_w_symbols <- merge(txs_wo_symbols, gene_symbols, by.x = "SHORTORF", by.y = "ORF")
  } else {
    gene_symbols <- suppressMessages(select(org_db, keys = genes_in_range$gene_id, columns = assembly$display.column, keytype = assembly$gene.id.column))
    txs_w_symbols <- merge(txs_wo_symbols, gene_symbols, by.x = "GENEID", by.y = assembly$gene.id.column)
  }

  return(txs_w_symbols)
}
